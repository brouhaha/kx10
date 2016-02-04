/* BPF Ethernet driver for kx10, the PDP-10 emulator.
   Copyright (C) 1994  Stu Grossman

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
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <net/pfilt.h>
#include <net/bpf.h>
#include <string.h>

static int pkf;			/* Packet filter fd */

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
      buflen = read (pkf, buf, sizeof buf);

      if (buflen < 0)
	{
	  perror ("Read error");
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

int
init_ether (void)
{
  int val;
  u_int uval;
  u_short bits;
  struct endevp endevp;
  struct timeval timeval;
  extern unsigned char myether[];

  static struct bpf_insn filters[] =
    {
      /* IP packets destined for me */
      BPF_STMT (BPF_LD+BPF_H+BPF_ABS, 12), /* IP packet? */
      BPF_JUMP (BPF_JMP+BPF_JEQ+BPF_K, ETHERTYPE_IP, 0,	3),

      BPF_STMT (BPF_LD+BPF_W+BPF_ABS, 30), /* Dest IP address */
      BPF_JUMP (BPF_JMP+BPF_JEQ+BPF_K, 0x8cae0107, 0, 1),

      BPF_STMT (BPF_RET+BPF_K, 1), /* Accept */

      /* Or, ARP requests or replies */
      BPF_JUMP (BPF_JMP+BPF_JEQ+BPF_K, ETHERTYPE_ARP, 0, 3), /* Arp? */

      BPF_STMT (BPF_LD+BPF_H+BPF_ABS, 20), /* Opcode */
      BPF_JUMP (BPF_JMP+BPF_JSET+BPF_K, ~3, 1, 0), /* Arp request or reply? */

      BPF_STMT (BPF_RET+BPF_K, 1), /* Accept */

      BPF_STMT (BPF_RET+BPF_K, 0) /* Reject */
    };

  static struct bpf_program filters_program =
    {sizeof filters / sizeof (struct bpf_insn), filters };

  pkf = pfopen (NULL, O_RDWR);

  if (pkf < 0)
    {
      perror ("pkopen failed");
      return 0;
    }

  /* Get our ethernet address */
  if (ioctl (pkf, EIOCDEVP, &endevp))
    {
      perror ("EIOCDEVP failed");
      close (pkf);
      return 0;
    }

  memcpy (myether, endevp.end_addr, 6);

  /* We only support 10mb ethernet for now */

  if (ioctl (pkf, BIOCGDLT, &val)
      || val != DLT_EN10MB)
    {
      perror ("BIOLGDLT failed");
      close (pkf);
      return 0;
    }

  /* Copy all local packets, enable batch mode, use BPF headers */
  bits = ENBATCH | ENCOPYALL | ENBPFHDR;
  if (ioctl (pkf, EIOCMBIS, &bits))
    {
      perror ("EIOCMBIS failed");
      close (pkf);
      return 0;
    }

  /* Give us a SIGUSR1 upon receipt of a packet */

  signal (SIGUSR1, SIG_IGN);	/* Disable receive process */

  uval = SIGUSR1;
  if (ioctl (pkf, EIOCENBS, &uval))
    {
      perror ("EIOCENBS failed");
      close (pkf);
      return 0;
    }

  /* Make the kernel's receive queue bigger */

  uval = 32;
  if (ioctl (pkf, EIOCSETW, &uval))
    {
      perror ("EIOCSETW failed");
      close (pkf);
      return 0;
    }

  /* Set non-blocking reads */

  timeval.tv_sec = -1;
  timeval.tv_usec = -1;

  if (ioctl (pkf, EIOCSRTIMEOUT, &timeval))
    {
      perror ("EIOCSRTIMEOUT failed");
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

  return 1;
}

