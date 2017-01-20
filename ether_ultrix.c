/* Ultrix 4.4 Ethernet driver for kx10, the PDP-10 emulator.
   Copyright (C) 1995  Stu Grossman
   This code is written by Bjorn Victor, based on Stu's bpf code.

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

/* Ethernet driver for Ultrix 4.4 packet filter */

#include "pdp10.h"
#include <sys/time.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <string.h>
#include <sys/errno.h>
#include <signal.h>
#include <errno.h>
#include <net/pfilt.h>

extern unsigned char myether [];
static unsigned long myipaddr;
static char net_device[128];

static int if_fd;

#define BUF_LEN (1550)

int
get_packet (packetp)
     unsigned char **packetp;
{
  static unsigned char buf[32 * (1550 + sizeof (struct enstamp))];
  static unsigned char *current_packet = buf;
  static int buflen = 0;
  static struct enstamp *bpf_hdr;

  if (current_packet >= buf + buflen)
    {
      buflen = read (if_fd, buf, sizeof buf);

      if (buflen < 0)
	{
	  perror ("get_packet: Read error");
	  return 0;
	}
      if (buflen == 0)
	return 0;

      current_packet = buf;
    }

  bpf_hdr = (struct enstamp *)current_packet;

  *packetp = current_packet + bpf_hdr->ens_stamplen;

  current_packet += ENALIGN (bpf_hdr->ens_stamplen + bpf_hdr->ens_count);

  return bpf_hdr->ens_count;
}

void
send_packet (packet, packetlen)
     unsigned char *packet;
     int packetlen;
{
  int cc;

  cc = write (if_fd, packet, packetlen);

#if 0
  printf ("send_packet: sending %d bytes: ", totlen);
  {
    int i;
    for (i = 0; i < totlen; i++)
      printf ("%02x ", packet[i]);
    printf ("\r\n");
  }
#endif

  if (cc == packetlen)
    return;

  if (cc >= 0)
    printf ("send_packet sent only %d bytes\r\n", cc);
  else
    {
      perror ("send_packet");
      fprintf (stderr, "\r");
    }
}

static void getconf PARAMS ((void));

static void
getconf ()
{
  FILE *f;
  char line[1000];
  struct ether_addr *ea, *ether_aton();
  struct in_addr ia;

  if ((f = fopen ("NetConfig", "r")) == NULL)
    return;

  net_device[0] = '\0';		/* clear it */
  memset (myether, '\0', 6);	/* clear it */

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

  ia.s_addr = myipaddr;

  fprintf(stderr,"Net Config:\n net %s\n ip %s\n ether %x:%x:%x:%x:%x:%x\n",
	  net_device[0] == '\0' ? "(default)" : net_device,
	  inet_ntoa (ia),
	  myether[0], myether[1], myether[2], myether[3], myether[4], myether[5]);
}

/* This is based on the bpf driver by Stu,
   and on code from ARNS (A Remote Network Server for AppleTalk).
*/

int
init_ether ()
{
  struct enfilter pf;
  register u_short *fwp = pf.enf_Filter;
  u_int uval;
  u_short bits;
  struct timeval timeval;
  struct endevp endev;
  

  getconf ();
  if (/*(net_device[0] == '\0') ||*/ (myipaddr == 0))
    {
      fprintf (stderr, "init_ether: not configured\n");
      return 0;
    }

  if ((if_fd = pfopen (net_device[0] == '\0' ? NULL : net_device, O_RDWR)) < 0)
    {
      perror ("init_ether: open device");
      return 0;
    }

  if (ioctl (if_fd, EIOCDEVP, &endev) < 0)
    {
      perror ("init_ether: EIOCDEVP failed");
      close (if_fd);
      return 0;
    }

  if (endev.end_dev_type != ENDT_10MB)
    {
      fprintf(stderr,"init_ether: unsupported interface type %d\n",
	      endev.end_dev_type);
      close (if_fd);
      return 0;
    }


  if (memcmp(myether, endev.end_addr, 6) != 0)
    {
      if ((myether[0] != 0) || (myether[1] != 0) || (myether[2] != 0)
	  || (myether[3] != 0) || (myether[4] != 0) || (myether[5] != 0))
	fprintf(stderr,
		"init_ether: bad ether address given, should be %x:%x:%x:%x:%x:%x\n",
		endev.end_addr[0], endev.end_addr[1], endev.end_addr[2],
		endev.end_addr[3], endev.end_addr[4], endev.end_addr[5]);

      memcpy(myether, endev.end_addr, 6);
    }

  /* Copy all local packets, enable batch mode, use headers */
  /* XXXX I'm not sure this is good:
     timestamping takes a lot of time says the paper /BV */
  bits = ENBATCH | ENCOPYALL | ENTSTAMP;
  if (ioctl (if_fd, EIOCMBIS, &bits))
    {
      perror ("EIOCMBIS failed");
      close (if_fd);
      return 0;
    }

  /* Give us a SIGUSR1 upon receipt of a packet */

  signal (SIGUSR1, SIG_IGN);	/* Disable receive process */

  uval = SIGUSR1;
  if (ioctl (if_fd, EIOCENBS, &uval))
    {
      perror ("init_ether: EIOCENBS failed");
      close (if_fd);
      return 0;
    }

  /* Make the kernel's receive queue bigger */

  uval = 32;
  if (ioctl (if_fd, EIOCSETW, &uval))
    {
      perror ("init_ether: EIOCSETW failed");
      close (if_fd);
      return 0;
    }

  /* Set non-blocking reads */

  timeval.tv_sec = -1;
  timeval.tv_usec = -1;

  if (ioctl (if_fd, EIOCSRTIMEOUT, &timeval))
    {
      perror ("init_ether: EIOCSRTIMEOUT failed");
      close (if_fd);
      return 0;
    }

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
  *fwp++ = htons((htonl (myipaddr)) >> 16);
  *fwp++ = ENF_PUSHWORD + 9 + sizeof (struct ether_header) / sizeof (u_short); /* IP dest address field part 2 */
  *fwp++ = ENF_PUSHLIT | ENF_EQ; /* and second part */
  *fwp++ = htons((htonl (myipaddr)) & 0xffff);


  pf.enf_FilterLen = fwp - &pf.enf_Filter[0];
  if (pf.enf_FilterLen > ENMAXFILTERS)
    {
      fprintf (stderr, "init_ether: Too long filter\n");
      return 0;
    }
    
  /* Install the filter */
  if (ioctl (if_fd, EIOCSETF, &pf) < 0)
    {
      perror ("init_ether: install packet filter");
      close (if_fd);
      return 0;
    }

  /* Flush the read queue, to get rid of anything that accumulated
     before the device reached its final configuration. */

  if (ioctl (if_fd, EIOCFLUSH, 0) < 0)
    {
      perror ("init_ether: FLUSH");
      return 0;
    }

  return 1;
}
