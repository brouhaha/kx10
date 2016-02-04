/* Main emulator loop for kx10, the PDP-10 emulator.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996  Stu Grossman

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

/* The main emulator !!! */

#include "pdp10.h"

#if 0
word36 zero36 = {0,0};
word36 one36 = {0777777,0777777};
#else
word36 zero36;
word36 one36;
#endif

addr10 pchist[1024];		/* PC history buffer */
int pchp = 0;			/* Pointer into pchist */
int debug_monitor;		/* > 0 means to debug kernel */
int patch_monitor = 1;		/* != 0 means install dte patches */
int waits_flag = 0;		/* != 0 means booting a Waits .dmp file */
int icount, intcount;
int boot_flags = 0;

/*static int __main () {return 0;}*/

/* Setup the processor, resets the 'pager' and the channels, etc... */

void
processor_initialize()
{
  currentacblock = acfile;	/* Reset to ac block 0 */
  prevacblock = acfile;
  mem_acblock = currentacblock;
  mem_acblock_1 = currentacblock;
  mem_acblock_2 = currentacblock;
  ea_acblock = currentacblock;

  pager_init ();		/* Clear tlbs, setup for exec VM accesses */

  interrupt = -1;

  io_init();			/* Setup I/O devices */

  console_init();
}

static void
handle_fpe ()
{
  printf("\r\nSIGFPE\r\n");
  genunimp ();
}

#if 0
int
set_cry0()
{
  if (!(pcflags & 04000))
    printf ("S:%o ", pc);

  return 04000;
}

int
set_cry1()
{
/*  printf ("S:%o ", pc);*/

  return 02000;
}
#endif

void
execute(start_address)
     addr10 start_address;
{
  register addr10 ea;		/* Effective address */
  register int opcode;
  word36 ir;

  processor_initialize();

  if (boot_flags)
    {
      dpb (17, 18, 0400000, acref(0)); /* No prompting */
      dpb (35, 18, 0, acref(0));
      start_address++;
    }

#if 1
  /* The following crock fools TOPS-20 into thinking that there is a 1 page
     copy of the boot code at the top of section 0.  Without this ruse, the
     monitor gets very confused. */

  dpb(17, 18, -1, ir);
  dpb(35, 18, 0777000, ir);
  pstore(0204, ir);
#endif

  if (patch_monitor != 0)
    {
      jam_input ("proflg[0\r");
      jam_input ("dteq[ret\r");
    }

  if (!waits_flag && debug_monitor > 0)
    {
      jam_input ("1\0331m");
      jam_input ("stg\033:");
      jam_input ("dbugsw[2\r");
      jam_input ("eddtf[1\r");
    }

#ifdef PCHIST
  opdisp[0777] = i_trace;
#endif
#ifdef PROCESS_TRACING
  opdisp[0777] = i_trace;
#endif

  signal (SIGFPE, handle_fpe);

  pcsection = 0;

  pc = start_address;

  /* siglongjmp to here when we need to abort the current instruction.  This is
     usually done for page faults, but can also occur in xct loops, or EA calc
     loops. */
  sigsetjmp (to_main, 0);

  while (1)
    {
      extern int trace_ubr;

      if (interrupt >= 0)
	{
/*	  intcount++;*/
	  handle_interrupt();
	}
/*      icount++;*/

#ifdef PCHIST
#if 0
      if (pcflags & PC_USER)
#endif
	pchist[pchp++ % 1024] = pcflags << 23 | pcsection | pc;

#if 1
      if (pc < 020)
	genunimp ();
#endif
#if 0
      if (pc == 0572622) {
	printf("Got to here\r\n");
	/*      genunimp();*/
      }
#endif /* undef */
#endif /* PCHIST */

#if 0
      if ((pcflags & PC_USER)
	  && (pc == 0505672)
	  && (pcsection == 01000000))
	printf ("Got to here\r\n");
#endif

      /* 
	Note that all PC refs are implicitly LOCAL.  This causes the PC to wrap
	around at section boundaries.  It also means that PC refs to 0-17 in any
	section really refer to the ACs.
	*/

      vfetch_i (pcsection | pc, ir);

#ifdef PROCESS_TRACING
      if (pagerdatailow == trace_ubr
	  && pcflags & PC_USER)
	{
	  printf ("%o: ", pcsection | pc);
	  print_instruction (ir);
	}
#endif

      incrpc;

      ea = ieacalc(ir, pcsection);

      opcode = ldb(8, 9, ir);
      (*opdisp[opcode])(opcode, ldb(12, 4, ir), ea);
    }
}

int
main (argc, argv)
     int argc;
     char *argv[];
{
  addr10 entvec;
  char *sysname;
  extern int tops10_paging;

  cfg = ConfigFileOpen ("kx10.config");

  output_version ();

#if 1
  zero36.hi = 0;
  zero36.lo = 0;
  one36.hi = 0777777;
  one36.lo = 0777777;
#endif

/*  memarray = (word36 *)calloc(2000000, sizeof(word36));*/

  entvec = -1;

  argc--;
  argv++;

  for (;argc > 0; argc--, argv++)
    if (argv[0][0] == '-')
      switch (argv[0][1])
	{
	case 'b':		/* non-dialog (boot.exe) */
	  boot_flags = 1;
	  break;
	case 'd':
	  debug_monitor++;
	  break;
	case 'n':
	  patch_monitor = 0;
	  debug_monitor = 0;
	  break;
	case 'w':
	  waits_flag = 1;
	  patch_monitor = 0;
	  break;
	case 't':
	  tops10_paging = 1;
	  break;
	default:
	  fprintf (stderr, "Usage: kx10 [-b] [-d] [-n] [-w] [executable ...]\n");
	  exit (1);
	}
      else
	entvec = readexe (*argv);

  if (entvec == -1)
    readexe ("monitr.exe");

  if (!waits_flag && (debug_monitor || patch_monitor))
    entvec = (addr10)0142;	/* Start address of EDDT */

  fprintf (stderr, "[Starting at 0%o]\n", entvec);
  execute (entvec);

  return 0;			/* Never actually gets here */
}
