/* Console I/O routines for kx10, the PDP-10 emulator.
   Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996
   Stu Grossman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
    
#include "pdp10.h"
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/errno.h>
#include <sys/time.h>

#ifdef USE_STREAMS
#include <sys/stropts.h>
#endif

extern int errno;
static void alarm_handler();

static volatile char consbuf[400]; /* Console input buffer */
static volatile char *consiptr = consbuf; /* Next place to put a newly arrived char */
static volatile char *consoptr = consbuf; /* Next place to get the oldest char */
static struct termios otermios;	/* Saved tty variables */
static int oflags;

static int console_fd = -1;

/* The type of protocol to simulate when talking with the -10 program.  primary
   is a shared-memory protocol used under timesharing mostly for communicating
   with front-end ttys.  secondary is a much simpler semaphore-based protocol
   which is used during booting and debugging (EDDT).  kldcp is very similar to
   secondary, but doesn't use the DTE doorbell mechanism.  The -10 is expected
   to poll for typein.  It is used only by KLDCP and WAITS.  */
   
enum fe_protocol_type {primary, secondary, kldcp};
static enum fe_protocol_type fe_protocol = secondary;

/* If zero, then most console memory accesses are relative to page zero.  If
   non-zero, then console memory accesses are relative to the exec page table.
   WAITS wants things to be EBR relative, whereas KLDCP wants things to be
   relative to page zero.  */

static int ebr_relative = 1;

void
console_reset ()
{
  signal (SIGIO, SIG_IGN);
  signal (SIGVTALRM, SIG_IGN);
  if (tcsetattr (console_fd, TCSADRAIN, &otermios))
    printf ("tcsetattr() failed, errno=%d\n", errno);
  fcntl (console_fd, F_SETFL, oflags);
}

void
suspend ()
{
#ifdef SIGSTOP
  kill(getpid(), SIGTSTP);
#else
  exit();
#endif
}

/*
 * Read a character from the console and return it as the return value.  If
 * there are no characters available, then return -1.
 */

static int
read_from_console()
{
  int cc;
  char cbuf;

#ifdef __osf__
  {
/* This little hack is necessary because OSF1 will generate a SIGIO each time a
   read is done from the terminal.  This causes us to get stuck in the SIGIO
   signal handler (input_interrupt).  The workaround is to avoid doing the
   reads unless data is available.  */
#include <sys/ioctl.h>
    int val;

    cc = ioctl (console_fd, FIONREAD, &val);

    if (cc)
      printf ("ioctl failed, errno=%d\r\n", errno);

    if (val <= 0)
      return -1;
  }
#endif

  cc = read (console_fd, &cbuf, sizeof(cbuf));

  if (cc <= 0)
    {
      if (cc == 0 || errno == EWOULDBLOCK)
	return -1;
      printf ("Error reading from tty, errno = %d\n", errno);
      console_reset ();
      exit (1);
    }

  return(cbuf & 0177);
}

static void
write_to_console (c)
     int c;
{
  char buf;

  buf = c;

  while (1)
    {
      int cc;

      cc = write (console_fd, &buf, 1); /* Output a character */

      if (cc > 0)
	break;

      if (cc == 0
	  || (cc < 0 && errno == EWOULDBLOCK))
	continue;

      perror ("write_to_console: write failed");
      console_reset ();
      exit (1);
    }
}

/* Put a character into the console input buffer */

static void
putibuf (c)
     int c;
{
  *consiptr++ = c;
  if (consiptr >= consbuf + sizeof(consbuf))
    consiptr = consbuf;
}

/* Jam the input string into the console input buffer */

void
jam_input (p)
     char *p;
{
  while (*p != '\000')
    putibuf (*p++);
  set_interrupt (0, DTE_BIT);
}

/*
 * Come here when we get a SIGIO for the console.  We read as much stuff as
 * possible into the consbuf, and then post an interrupt if the buffer is
 * non-empty.
 */

static int console_flag = 0;	/* Non-zero means next char is csl func */

static void
input_interrupt()
{
  int c;

  while (1)
    {
      c = read_from_console();
      if (c == -1)
	break;
      if (console_flag)
	{
	  word36 mem;

	  console_flag = 0;
	  switch (c)
	    {
	    case '?':
	      printf ("Console commands:\r\n\
	^E - Enter EDDT\r\n\
	 s - Deposit 1 in loc 030 (shutdown)\r\n\
	^T - Status\r\n\
	^\\ - Insert ^\\\r\n\
	 q - Quit (actually suspend) kx10\r\n");
	      break;
	    case '\005':	/* ^E for EDDT */
#ifdef KLDCP
	      pfetch (030051, mem);
	      printf ("testpc: 0%o\r\n", lower18(mem));
#endif
	      printf("PC/Flags: %o,,%o, Interrupts: %d\r\n",
		     (pcflags << 5) | (pcsection >> 18), pc, interrupt);
	      pcsection = 0;
	      pcflags = 0;
	      goexec ();
#ifndef KLDCP
	      pfetch (074, mem); /* .jbddt */
	      pc = ldb (35, 18, mem);
#else
	      pc = 010000;
#endif
	      break;
	    case 's':		/* loc 30 shutdown */
	    case 'S':
	      dpb (17, 18, 0, mem);
	      dpb (35, 18, 1, mem);
	      pstore (030, mem);
	      break;
	    case '\024':	/* ^T for status info */
	      {
		sigset_t oldmask;

		sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

		printf ("PC/Flags: %o,,%o",
		       (pcflags << 5) | (pcsection >> 18), pc);

		print_interrupts ();
		printf ("\r\n");

		sigprocmask (SIG_SETMASK, &oldmask, NULL);
	      }
	      break;
	    case '\034':	/* Self insert */
	      putibuf (c);
	      break;
	    case 'q':		/* q for quit or suspend */
#ifdef PCHIST
	      ppc ();
#endif
	      console_reset();
	      printf("\n");
	      /*      exit(0);*/
	      suspend();
	      console_init();
	      break;
	    }
	}
      else if (c == '\034')
	console_flag = 1;	/* ^\ does console mode */
      else
	putibuf (c);
    }

  if (fe_protocol == secondary && consiptr != consoptr)
    set_interrupt(0, DTE_BIT);
}

void
console_init ()
{
  struct termios termios;
  struct sigaction action;
  sigset_t nullsigmask = {0};
  int newflags;

  action.sa_handler = input_interrupt;
  action.sa_mask = nullsigmask;
  action.sa_flags = SA_RESTART;
  sigaction (SIGIO, &action, NULL);

  action.sa_handler = alarm_handler;
  sigaction (SIGVTALRM, &action, NULL);

  console_fd = open ("/dev/tty", O_RDWR);

  if (console_fd < 0)
    {
      perror ("console_init: open failed");
      exit (1);
    }

  if (tcgetattr (console_fd, &termios)) {
    printf ("tcgetattr() failed: errno=%d\n", errno);
    exit (1);
  }

  otermios = termios;

  termios.c_iflag = 0;
  termios.c_oflag = 0;
  termios.c_lflag = 0;
  termios.c_cflag &= ~(CSIZE|PARENB);
  termios.c_cflag |= CS8;
  termios.c_cc[VMIN] = 0;
  termios.c_cc[VTIME] = 0;

  if (tcsetattr(console_fd, TCSADRAIN, &termios)) {
    printf("tcsetattr() failed: errno=%d\r\n", errno);
    exit(1);
  }

  fcntl (console_fd, F_SETOWN, getpid());

  oflags = fcntl (console_fd, F_GETFL, 0);

#ifdef USE_STREAMS

  /* Enable SIGIO/SIGPOLL */

  if (ioctl (console_fd,  I_SETSIG, S_INPUT|S_RDNORM) < 0)
    {
      printf ("I_SETSIG failed: %d\r\n", errno);
      console_reset ();
      exit (1);
    }
  newflags = oflags | O_NDELAY;	/* Turn on non-blocking reads */
#else
  /* Enable SIGIO and non-blocking for non-streams ttys */
  newflags = oflags | FNDELAY | FASYNC;
#endif

  fcntl (console_fd, F_SETFL, newflags);
}

/*
 * Set an alarm for some number of milliseconds in the future.  This routine
 * is identical to alarm(3), except that it takes it's arg in ms.
 */

static void
alarm_ms (time)
     int time;
{
  struct itimerval when;

  timerclear (&when.it_interval);

  when.it_value.tv_sec = time / 1000;
  when.it_value.tv_usec = (time % 1000) * 1000;

  setitimer (ITIMER_VIRTUAL, &when, NULL);
}

static void
alarm_handler ()
{
  set_interrupt (0, DTE_BIT);
}

/* Return time in local time zone.  Note that time is not corrected for DST.  */

static void
read_clock ()
{
  struct tm *tm;
  time_t clock;
  word36 mem;

  clock = time (NULL);

  tm = localtime (&clock);

  if (!tm)
    return;

  if (tm->tm_isdst)
    {
      clock -= 3600;		/* Compensate for DST */
      tm = localtime (&clock);
    }

  dpb (4, 5, tm->tm_mday, mem);
  dpb (6, 2, 0, mem);
  dpb (11, 5, tm->tm_hour, mem);
  dpb (13, 2, 0, mem);
  dpb (19, 6, tm->tm_min, mem);
  dpb (23, 4, tm->tm_mon + 1, mem);
  dpb (27, 4, 10, mem);		/* Year? */
  dpb (29, 2, 0, mem);
  dpb (35, 6, tm->tm_sec, mem);

  if (ebr_relative)
    estore (0446, mem);
  else
    pstore (0446, mem);
}

/*
 * Come here when the -10 rings the DTE's doorbell.  We look for a command
 * and execute it.
 */

void
dte_doorbell ()
{
  word36 mem;
  unsigned char cmd, arg;
  sigset_t oldmask;
  int c;

  if (ebr_relative)
    efetch(0451, &mem);		/* Get DTECMD */
  else
    pfetch(0451, mem);		/* [KLDCP] Get DTECMD */

  cmd = ldb(27, 8, mem);	/* Get the command byte */
  arg = ldb(35, 8, mem);	/* Get the argument */

  switch(cmd)
    {
    case 0:			/* [KLDCP] Print a char */
      arg &= 0177;		/* Lop off top bit */
      write_to_console (arg);	/* Output a character */
      break;
    case 01:			/* [KLDCP] Processor control */
      switch (arg)
	{
	case 03:		/* End of program */
	  printf ("End of program.\r\n");
	  genunimp ();
	  break;
	case 04:		/* End of pass */
	  printf ("End of pass.\r\n");
	  genunimp ();
	  break;
	case 05:		/* Get main clock control */
	  if (ebr_relative)
	    estore (0450, zero36);
	  else
	    pstore (0450, zero36);
	  break;
	default:
	  printf ("Unimplemented fe processor control function %o\n", arg);
	  genunimp();
	}
      break;
    case 02:			/* [KLDCP] Various clock functions */
      switch (arg)
	{
	case 04:		/* Set FE date/time */
	  break;		/* NOT! */
	case 06:		/* Read date/time into 0446 */
	  read_clock ();
	  break;
	default:
	  printf ("Unimplemented fe clock function = %o\n", arg);
	  genunimp();
	}
      break;
    case 03:			/* [KLDCP] Switch functions */
      switch (arg)
	{
	case 0:			/* Read switches (always 405 for now) */
	  mem = zero36;
	  dpb (35, 18, 0405, mem);
	  if (ebr_relative)
	    estore (0450, mem);
	  else
	    pstore (0450, mem);
	  break;
	default:
	  printf ("Unimplemented fe switch function %o\n", arg);
	  genunimp ();
	}
      break;
    case 06:			/* [KLDCP] Force tty output? */
      break;
    case 07:			/* [KLDCP] tty poll */
      sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);
      if (consoptr != consiptr)
	{
	  c = *consoptr++;	/* Fetch the oldest character */
	  if (consoptr >= consbuf + sizeof(consbuf))
	    consoptr = consbuf;
	}
      else
	c = 0;
      dpb (35, 8, c, mem);	/* Install the input character */
      if (ebr_relative)
	estore (0450, mem);	/* Put char where -10 can find it (DTEF11) */
      else
	pstore (0450, mem);	/* Put char where -10 can find it (DTEF11) */
      sigprocmask (SIG_SETMASK, &oldmask, NULL);
      break;
    case 010:			/* Print a char in secondary protocol */
      arg &= 0177;		/* Lop off top bit */
      write_to_console (arg);	/* Output a character */
      estore (0455, one36);	/* Set the done flag for chars (DTEMTD) */
      dteconi |= DTETO10DB;	/* Ding dong */
      if (dteconi & 07)
	set_interrupt(dteconi, DTE_BIT);
      return;
    case 011:			/* Enter secondary protocol */
#ifdef undef
      printf("[Entering secondary protocol]\r\n"); /* spread the bullshit around liberally */
#endif /* undef */
      fe_protocol = secondary;
      break;
    case 012:			/* [KLDCP] Enter kldcp protocol */
      fe_protocol = kldcp;
      break;
    case 013:			/* [KLDCP] Read monitor state */
      mem = zero36;
      dpb (35, 8, fe_protocol != kldcp, mem);
      if (ebr_relative)
	estore (0450, mem);
      else
	pstore (0450, mem);
      break;
    default:
      printf("unimplemented fe command cmd = %o, arg = %o\n", cmd, arg);
      genunimp();
    }

  if (ebr_relative)
    estore(0444, one36);		/* Set the command done flag (DTEFLG) */
  else
    pstore(0444, one36);		/* [KLDCP] Set the command done flag (DTEFLG) */
}

void
dte_interrupt ()
{
  int c;
  word36 mem;
  sigset_t oldmask;

  sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

  clear_interrupt (0, DTE_BIT);

  if (consiptr != consoptr)
    {
      efetch(0456, &mem);	/* Fetch the magic location (DTEMTI) */
      if (ne36(mem))		/* Is the flag non-zero? */
	alarm_ms (100);		/* Yes, try again later */
      else
	{			/* Flag is clear */
	  c = *consoptr++;	/* Fetch the oldest character */
	  if (consoptr >= consbuf + sizeof(consbuf))
	    consoptr = consbuf;
	  dpb(35, 8, c, mem);	/* Install the input character */
	  estore(0450, mem);	/* Put char where -10 can find it (DTEF11) */
	  estore(0456, one36);	/* Set the flag (DTEMTI) */
	  /*    printf("Char: %o\r\n", c);
		fflush(stdout); */
	  if (consiptr != consoptr)
	    alarm_ms (100);	/* Set alarm if more input pending */
	}

      dteconi |= DTETO10DB;	/* Ding dong */
      if (dteconi & 07)
	set_interrupt (dteconi, DTE_BIT);
    }

  sigprocmask (SIG_SETMASK, &oldmask, NULL);
}
