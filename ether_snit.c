/* SNIT Ethernet driver for kx10, the PDP-10 emulator.
   Copyright (C) 1995  Stu Grossman
   This code is written by Bjorn Victor, don't blame Stu!

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

/* Ethernet driver for SunOS 4 SNIT */

#include "pdp10.h"
#include <sys/time.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/sockio.h>
#include <sys/filio.h>
#include <net/route.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <string.h>
#include <sys/errno.h>
#include <signal.h>
#include <errno.h>

#define	DEV_NIT	"/dev/nit"
#include <sys/file.h>
#include <sys/ioctl.h>
#include <net/nit.h>
#include <net/nit_if.h>
#include <net/nit_pf.h>
#include <net/packetfilt.h>
#include <stropts.h>

extern unsigned char myether [];
static unsigned long myipaddr;
static char net_device[128];

static int if_fd;

#define BUF_LEN (1550)

int
get_packet (packetp)
     unsigned char **packetp;
{
  int buflen;
  static unsigned char buf[BUF_LEN][32], *b;
  static int bidx = 0;
  struct sockaddr sa;
  struct strbuf pbuf, dbuf;
  int val, flags = 0;

  b = buf[bidx];

  pbuf.maxlen = sizeof (sa);
  pbuf.buf = (char *)&sa;
  dbuf.maxlen = BUF_LEN;
  dbuf.buf = (char *)b;

  if ((val = getmsg (if_fd, &pbuf, &dbuf, &flags)) != 0)
    {
      if (val < 0)
	{
	  if (errno != EINTR
	      && errno != EAGAIN)
	    perror("getmsg");
	}
      return 0;
    }
  else
    {
      bidx++;
      bidx %= 32;
      *packetp = b;

      return dbuf.len;
    }
}

void
send_packet (packet, packetlen)
     unsigned char *packet;
     int packetlen;
{
  struct sockaddr sa;
  struct strbuf pbuf, dbuf;

  sa.sa_family = AF_UNSPEC;
  memcpy (sa.sa_data, packet, sizeof(sa.sa_data));
  pbuf.len = sizeof (struct sockaddr);
  pbuf.buf = (char *) &sa;
  dbuf.len = packetlen - 14;
  dbuf.buf = (char *)packet + 14;
  if (putmsg (if_fd, &pbuf, &dbuf, 0) == 0)
    return;
  else
    {
      perror ("send_packet");
      fprintf (stderr, "\r");
    }
  return;
}

static void getconf PARAMS ((void));

static void
getconf ()
{
  FILE *f;
  char line[1000];
  struct ether_addr *ea, *ether_aton();
  
  if ((f = fopen ("NetConfig", "r")) == NULL)
    return;

  for (fgets (line, sizeof (line), f);
       !feof (f);
       fgets (line, sizeof (line), f))
    {
      if (line[0] == '#')
	;
      else if (strncmp (line, "ether ", 6) == 0)
	{
	  if ((ea = ether_aton (&line[6])) != NULL)
	    memcpy(myether,ea->ether_addr_octet,6);
	}
      else if (strncmp (line, "ip ", 3) == 0)
	{
	  if ((myipaddr = inet_addr (&line[3])) == -1)
	    myipaddr = 0;
	}
      else if (strncmp (line, "net ", 4) == 0)
	{
	  line[strlen (line) - 1] = '\0';
	  strcpy (net_device, &line[4]);
	}
      else
	fprintf (stderr, "unknown NetConfig keyword: %s\n", line);
    }

  fprintf(stderr,"Net Config:\n net %s\n ip %d.%d.%d.%d\n ether %x:%x:%x:%x:%x:%x\n",
	  net_device,
	  myipaddr >> 24, (myipaddr >> 16) & 0xff,
	  (myipaddr >> 8) & 0xff, myipaddr & 0xff,
	  myether[0], myether[1], myether[2], myether[3], myether[4], myether[5]);
}

/* This is based on code in the SunOS manual, 
   and on code from ARNS (A Remote Network Server for AppleTalk).
*/

int
init_ether ()
{
  struct strioctl si;
  struct ifreq ifr;
  struct packetfilt pf;
  register u_short *fwp = pf.Pf_Filter;
  u_long if_flags = NI_LEN;

  getconf ();
  if ((net_device[0] == '\0') || (myipaddr == 0))
    {
      fprintf (stderr, "init_ether: not configured\n");
      return 0;
    }

  if ((if_fd = open (DEV_NIT, O_RDWR)) == -1)
    {
      perror ("init_ether: open NIT");
      return 0;
    }

  /* Arrange to get discrete messages from the stream. */

  if ((ioctl (if_fd, I_SRDOPT, (char *)RMSGD)) == -1)
    {
      perror ("init_ether: RMSGD");
      return 0;
    }

  /* Configure the nit device, binding it to the proper underlying interface. */

  strncpy (ifr.ifr_name, net_device, sizeof ifr.ifr_name);
  ifr.ifr_name[sizeof ifr.ifr_name - 1] = '\0';

  si.ic_cmd = NIOCBIND;
  si.ic_timout = INFTIM;
  si.ic_len = sizeof ifr;
  si.ic_dp = (char *)&ifr;
  if (ioctl (if_fd, I_STR, (char *)&si) == -1)
    {
      perror ("init_ether: bind");
      return 0;
    }

#if 0
  {
    int val = 1;
    if (ioctl(if_fd,FIONBIO,&val) == -1) {
      perror("init_ether: FIONBIO");
      return 0;
    }
    val = 1;
    if (ioctl(if_fd,FIOASYNC,&val) == -1) {
      perror("init_ether: FIOASYNC");
      return 0;
    }

    signal(SIGPOLL,SIG_IGN);

    if (ioctl(if_fd,I_SETSIG,S_INPUT) == -1) {
      perror("init_ether: I_SETSIG");
      return 0;
    }
  }
#endif


  /* Construct a packet filter */

#define	s_offset(structp, element) (&(((structp)0)->element))

  memset (&pf, '\000', sizeof(pf));

  *fwp++ = ENF_PUSHWORD + ((int)s_offset (struct ether_header *, ether_type)) / sizeof (u_short);
  *fwp++ = ENF_PUSHLIT | ENF_COR;
  *fwp++ = htons(ETHERTYPE_ARP); /* ARP packet => return T immediately */

  *fwp++ = ENF_PUSHWORD + ((int)s_offset (struct ether_header *, ether_type)) / sizeof (u_short);
  *fwp++ = ENF_PUSHLIT | ENF_CAND;
  *fwp++ = htons(ETHERTYPE_IP); /* IP packet? Continue, else fail imm. */

  *fwp++ = ENF_PUSHWORD + 8 + sizeof (struct ether_header) / sizeof (u_short); /* IP dest address field */
  *fwp++ = ENF_PUSHLIT | ENF_CAND; /* first part must match */
  *fwp++ = htons (myipaddr >> 16);
  *fwp++ = ENF_PUSHWORD + 9 + sizeof (struct ether_header) / sizeof (u_short); /* IP dest address field part 2 */
  *fwp++ = ENF_PUSHLIT | ENF_EQ; /* and second part */
  *fwp++ = htons (myipaddr & 0xffff);


  pf.Pf_FilterLen = fwp - &pf.Pf_Filter[0];
  if (pf.Pf_FilterLen > ENMAXFILTERS)
    {
      fprintf (stderr, "init_ether: Too long filter\n");
      return 0;
    }
    
  si.ic_cmd = NIOCSETF;
  si.ic_timout = INFTIM;
  si.ic_len = sizeof (pf);
  si.ic_dp = (char *)&pf;

  /* Push the packet filter module */

  if (ioctl (if_fd, I_PUSH, "pf") < 0)
    {
      perror ("init_ether: push pf");
      return 0;
    }

  /* Install the filter */

  if (ioctl (if_fd, I_STR, (char *)&si) < 0)
    {
      perror ("init_ether: install pf");
      return 0;
    }

  /* Flush the read queue, to get rid of anything that accumulated
     before the device reached its final configuration. */

  if (ioctl (if_fd, I_FLUSH, (char *)FLUSHR) < 0)
    {
      perror ("init_ether: FLUSH");
      return 0;
    }

  /* Give us a SIGUSR1 upon receipt of a packet */
    
#if 1
  signal (SIGUSR1, SIG_IGN);

  {
    int pid;
    int ppid;
    fd_set readfds;

    ppid = getpid ();
    FD_ZERO (&readfds);

    pid = fork ();
    if (pid == 0)
      {				/* child */
	int numfds;

	while (1)
	  {
	    FD_SET (if_fd, &readfds);
	    numfds = select (if_fd + 1, &readfds, 0, 0, 0);

	    if (numfds != 1)
	      {
		if (numfds == -1)
		  perror ("Ether Select: ");
		else
		  fprintf (stderr, "Ether: numfds = %d\n", numfds);

		_exit (1);
	      }

	    if (!FD_ISSET (if_fd, &readfds))
	      fprintf (stderr, "!FD_ISSET signal process\n");

	    if (kill (ppid, SIGUSR1))
	      _exit (0);	/* Parent went away.  Die quietly */
	  }
      }
  }				/* End select process */
#endif

  return 1;
}
