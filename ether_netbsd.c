/* BPF Ethernet driver for kx10, the PDP-10 emulator.
   Copyright (C) 1994, 1995, 1996  Stu Grossman

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

/* Ethernet driver for Berkeley Packet Filter */

#include "pdp10.h"
#include <sys/time.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/sockio.h>
#include <sys/filio.h>
#include <net/route.h>
#include <net/if.h>
#include <net/if_dl.h>
#include <net/if_types.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <net/bpf.h>
#include <string.h>
#include <sys/errno.h>

extern unsigned char myether[];

static int pkf;			/* Packet filter fd */
static int bpf_buflen;		/* Use this size or else reads fail! */

int
get_packet (packetp)
     unsigned char **packetp;
{
  static unsigned char buf[32 * (1550 + sizeof (struct bpf_hdr))];
  static unsigned char *current_packet = buf;
  static int buflen = 0;
  static struct bpf_hdr *bpf_hdr;

  if (current_packet >= buf + buflen)
    {
#if 0
      ioctl (pkf, FIONREAD, &buflen);
      if (buflen == 0)
	return 0;
#endif

      buflen = read (pkf, buf, bpf_buflen);

      if (buflen < 0)
	{
	  if (errno != EWOULDBLOCK)
	    perror ("get_packet: read() failed: ");
	  return 0;
	}
      if (buflen == 0)
	return 0;

      current_packet = buf;
    }

  bpf_hdr = (struct bpf_hdr *)current_packet;

  *packetp = current_packet + bpf_hdr->bh_hdrlen;

  current_packet += BPF_WORDALIGN (bpf_hdr->bh_hdrlen + bpf_hdr->bh_caplen);

  return bpf_hdr->bh_caplen;
}

void
send_packet (packet, packetlen)
     unsigned char *packet;
     int packetlen;
{
  int cc;

  cc = write (pkf, packet, packetlen);

#if 0
  printf ("send_packet: sending %d bytes: ", packetlen);
  {
    int i;
    for (i = 0; i < packetlen; i++)
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

static int
pfopen (name_ignored, flags)
     char *name_ignored;
     int flags;
{
  char name[100];
  int i, tmp;
  int fd, sockfd;
  struct ifreq ifreq[10];
  struct ifconf ifconf;
  struct ifreq tmp_ifreq;

  sockfd = socket (AF_INET, SOCK_DGRAM, 0);

  if (sockfd < 0)
    {
      perror ("pfopen: socket failed");
      return -1;
    }

  ifconf.ifc_len = sizeof (ifreq);
  ifconf.ifc_req = ifreq;
  i = ioctl (sockfd, SIOCGIFCONF, &ifconf);

  if (i < 0)
    {
      perror ("pfopen: SIOCGIFCONF failed");
      close (sockfd);
      return -1;
    }

  for (i = 0; i < ifconf.ifc_len;)
    {
      struct ifreq *ifreqp;
      struct sockaddr_dl *sdp;

      ifreqp = (struct ifreq *)((char *)ifreq + i);

      i += sizeof (ifreqp->ifr_name) + ifreqp->ifr_addr.sa_len;
      sdp = (struct sockaddr_dl *)&ifreqp->ifr_addr;

      if (sdp->sdl_family != AF_LINK
	  && sdp->sdl_type != IFT_ETHER
	  && sdp->sdl_alen != 6)
	continue;		/* Not an ethernet */

/* Stash away the ethernet address in case this is the one.  */

      memcpy (myether, &sdp->sdl_data[sdp->sdl_nlen], 6);

      tmp_ifreq = *ifreqp;

      tmp = ioctl (sockfd, SIOCGIFFLAGS, &tmp_ifreq);

      if (tmp < 0)
	{
	  perror ("pfopen: SIOCGIFFLAGS failed");
	  continue;
	}

#define ON_FLAGS ((short)(IFF_UP|IFF_BROADCAST|IFF_RUNNING|IFF_MULTICAST))
#define OFF_FLAGS ((short)(IFF_LOOPBACK|IFF_POINTOPOINT|IFF_NOARP))
#define CARE_FLAGS ((short)(ON_FLAGS|OFF_FLAGS))

/* Make sure that interface is on, not point-to-point, etc... */

      tmp_ifreq.ifr_flags &= CARE_FLAGS; /* Mask out don't cares */

      if (tmp_ifreq.ifr_flags == ON_FLAGS)
	break;			/* Found our interface! */
    }

  close (sockfd);

  if (i >= ifconf.ifc_len)
    return -1;			/* Couldn't find an interface */

  for (i = 0; i < 100; i++)
    {
      sprintf (name, "/dev/bpf%d", i);

      fd = open (name, flags);

      if (fd < 0)
	if (errno == ENOENT)
	  return -1;
	else
	  {
	    char buf[100];

	    sprintf (buf, "pfopen:  open of %s failed", name);
	    perror (buf);
	    continue;
	  }

      tmp = ioctl (fd, BIOCSETIF, &tmp_ifreq);

      if (tmp < 0)
	{
	  perror ("pfopen:  BIOCSETIF failed");
	  close (fd);
	  return -1;
	}
      return fd;
    }

  return -1;
}

int
init_ether (void)
{
  int val;
  u_int uval;
  u_short bits;
  struct timeval timeval;
  char *myipaddr;
  long tmp;

  static struct bpf_insn filters[] =
    {
      /* IP packets destined for me */
      BPF_STMT (BPF_LD+BPF_H+BPF_ABS, 12), /* IP packet? */
      BPF_JUMP (BPF_JMP+BPF_JEQ+BPF_K, ETHERTYPE_IP, 0,	3),

      BPF_STMT (BPF_LD+BPF_W+BPF_ABS, 30), /* Dest IP address */
      BPF_JUMP (BPF_JMP+BPF_JEQ+BPF_K, 0x8cae0107, 0, 1),

      BPF_STMT (BPF_RET+BPF_K, 2000), /* Accept */

      /* Or, ARP requests or replies */
      BPF_JUMP (BPF_JMP+BPF_JEQ+BPF_K, ETHERTYPE_ARP, 0, 3), /* Arp? */

      BPF_STMT (BPF_LD+BPF_H+BPF_ABS, 20), /* Opcode */
      BPF_JUMP (BPF_JMP+BPF_JSET+BPF_K, ~3, 1, 0), /* Arp request or reply? */

      BPF_STMT (BPF_RET+BPF_K, 2000), /* Accept */

      BPF_STMT (BPF_RET+BPF_K, 0) /* Reject */
    };

  static struct bpf_program filters_program =
    {sizeof filters / sizeof (struct bpf_insn), filters };

  myipaddr = ConfigGetHostAddress (cfg, NULL, "ip");
  if (!myipaddr)
    {
      fprintf (stderr, "\n[kx10.config needs to specify an ip address.]\n\n");
      return 0;
    }
  memcpy (&tmp, myipaddr, 4);
  tmp = ntohl (tmp);		/* Convert to host byte order */
  filters[3].k = tmp;		/* Install in filter */

  pkf = pfopen (NULL, O_RDWR);

  if (pkf < 0)
    {
      fprintf (stderr, "\n[KX10 couldn't find a BPF interface.  Disabling ethernet emulation.]\n\n");
      return 0;
    }

  /* We only support 10mb ethernet for now */

  if (ioctl (pkf, BIOCGDLT, &val)
      || val != DLT_EN10MB)
    {
      perror ("BIOLGDLT failed");
      close (pkf);
      return 0;
    }

  /* Give us a SIGUSR1 upon receipt of a packet */

  signal (SIGUSR1, SIG_IGN);	/* Disable receive process */

  uval = SIGUSR1;

  /* Set non-blocking reads */

  timeval.tv_sec = -1;
  timeval.tv_usec = -1;

  if (ioctl (pkf, BIOCSRTIMEOUT, &timeval))
    {
      perror ("BIOCSRTIMEOUT failed");
      close (pkf);
      return 0;
    }

  val = 1;
  if (ioctl (pkf, BIOCIMMEDIATE, &val))
    {
      perror ("BIOCIMMEDIATE failed");
      close (pkf);
      return 0;
    }

  /* Turn on the filters */
  if (ioctl (pkf, BIOCSETF, &filters_program))
    {
      perror ("BIOCSETF failed");
      close (pkf);
      return 0;
    }

  if (ioctl (pkf, BIOCGBLEN, &bpf_buflen))
    {
      perror ("BIOCGBLEN failed");
      close (pkf);
      return 0;
    }

#if 0
  {
    int pid;
    int ppid;
    fd_set readfds;

    ppid = getpid ();
    FD_ZERO (&readfds);

    pid = fork ();
    if (pid == 0)		/* child */
      {
	int numfds;

	while (1)
	  {
	    FD_SET (pkf, &readfds);
	    numfds = select(pkf + 1, &readfds, 0, 0, 0);

	    if (numfds != 1)
	      {
		if (numfds == -1)
		  perror ("Select: ");
		else
		  fprintf (stderr, "numfds = %d\n", numfds);

		_exit (1);
	      }

	    if (!FD_ISSET (pkf, &readfds))
	      fprintf (stderr, "!FD_ISSET signal process\n");

	    if (kill (ppid, SIGUSR1))
	      _exit (0);	/* Parent went away.  Die quietly */
	  }
      }
  }
#else
  val = 1;
  if (ioctl (pkf, FIONBIO, &val))
    {
      perror ("FIONBIO failed");
      close (pkf);
      return 0;
    }

  uval = SIGUSR1;
  if (ioctl (pkf, BIOCSRSIG, &uval))
    {
      perror ("BIOCSRSIG failed");
      close (pkf);
      return 0;
    }

  val = getpid();
  if (ioctl (pkf, FIOSETOWN, &val))
    {
      perror ("FIOSETOWN failed");
      close (pkf);
      return 0;
    }

  val = 1;
  if (ioctl (pkf, FIOASYNC, &val))
    {
      perror ("FIOASYNC failed");
      close (pkf);
      return 0;
    }

#endif

  return 1;
}
