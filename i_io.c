/* General I/O instructions for kx10, the PDP-10 emulator.
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

#include "pdp10.h"
#include <sys/time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <stdlib.h>

static void no_device PARAMS ((int device));

#define APRIOR	0200000		/* I/O reset */
#define	APRENA	0100000		/* Enable interrupts */
#define APRDIS	0040000		/* Disable interrupts */
#define APRCLR	0020000		/* Clear interrupts */
#define	APRSET	0010000		/* Set interrupts */

#define APRSBE	0004000		/* SBUS Error */
#define APRNXM	0002000		/* Non Existent Memory */
#define APRIOP	0001000		/* I/O page fail */
#define APRMBP	0000400		/* Memory Buffer Parity Error */
#define APRCDP	0000200		/* Cache Dir Parity Error */
#define APRAPE	0000100		/* Address Parity Error */
#define APRPOW	0000040		/* Power Failure */
#define APRSWD	0000020		/* Cache Sweep Done */
#define APRIRQ	0000010		/* Interrupt Request */

int aprpiena;			/* Mask of APR bits which can interrupt */
int aprconi;			/* APR coni/cono bits */
struct timeval boottime;	/* Start time of kx10 */
int timconi;

void
io_init ()
{
  extern int tops10_paging;
  const static int sigs[] = BLOCKEDSIGS;
  int i;

  sigemptyset (&blockedsigs);
  for (i = 0; i < sizeof sigs / sizeof sigs[0]; i++)
    sigaddset (&blockedsigs, sigs[i]);

  ppireq = 0;			/* Reset program PI requests */
  piconi = 0;			/* Turn off PI system, and reset it */

  if (!tops10_paging)
    pagerconi = PAGKL;		/* EBR is 0, and TOPS-20 style pager */
  pagerdataihigh = 0700000;	/* AC block 0, PC AC block 0, PCS 0 */
  pagerdatailow = 0;		/* UPT at page 0 */

  aprpiena = 0;			/* No APR interrupts for now */
  aprconi = 0;			/* Clear all APR status bits */

  dteconi = 010;		/* PI0 enabled, no PIA yet */

  mtrconi = 0;
  timconi = 0;

  gettimeofday (&boottime, 0);

  rh20_init ();

  no_device (0110);		/* Card punch */
  no_device (0120);		/* KA console */
  no_device (0204);		/* DTE1 */
  no_device (0210);		/* DTE2 */
  no_device (0214);		/* DTE3 */
  no_device (0270);		/* RH10 # 0 */
  no_device (0274);		/* RH10 # 1 */
  no_device (0354);		/* TM10 */
  no_device (0360);		/* RH10 # 2 */
  no_device (0364);		/* RH10 # 3 */
  no_device (0370);		/* RH10 # 4 */
  no_device (0374);		/* RH10 # 5 */
  no_device (0404);		/* strange WAITS device */
  no_device (0520);		/* AN20#0 */
  no_device (0524);		/* AN20#1 */
  no_device (0530);		/* another strange WAITS device */
  no_device (0544);		/* RH20 # 1 */
  no_device (0550);		/* RH20 # 2 */
  no_device (0554);		/* RH20 # 3 */
  no_device (0560);		/* RH20 # 4 */
				/* RH20 # 5 KNI */
  no_device (0570);		/* RH20 # 6 */
  no_device (0574);		/* RH20 # 7 */

  no_device (0774);		/* ?!?! */
}

/* Handle blki/blko to nonexistent device.  We go through the motions, but
   output data goes nowhere, and input data is always zero.  */

SPECIAL_IO_INST (blkio,nodev,000)
{
  word36 mem;
  addr10 addr;
  int count;

  vfetch (ea, mem);		/* Get pointer */

  count = upper18 (mem) + 1;
  addr = pcsection | ((lower18 (mem) + 1) & HWORDMASK);

  if (ac & 02)
    vfetch (addr, mem);		/* BLKO, fetch the output word */
  else
    vstore (addr, zero36);	/* BLKI, store the input word */

  if (count != 01000000)	/* Count overflow? */
    incrpc;			/* No, skip instruction */

  dpb (17, 18, count, mem);
  dpb (35, 18, addr, mem);

  vstore (ea, mem);
}

/* Define a non-existent device to prevent kx10 traps.  */

static void
no_device (device)
     int device;
{
  extern void i_setzm(), i_trn(), i_trna();

  if (device & 03)
    abort ();

  device <<= 1;			/* Make room for opcode */

  iodisp[device + 0] = i_blkio_nodev;	/* BLKI */
  iodisp[device + 1] = i_setzm;		/* DATAI */
  iodisp[device + 2] = i_blkio_nodev;	/* BLKO */
  iodisp[device + 3] = i_trn;		/* DATAO */
  iodisp[device + 4] = i_trn;		/* CONO */
  iodisp[device + 5] = i_setzm;		/* CONI */
  iodisp[device + 6] = i_trna;		/* CONSZ */
  iodisp[device + 7] = i_trn;		/* CONSO */
}

/* All I/O instructions come through here.  This does the second level dispatch
   through the io dispatch table.  */

SPECIAL_INST (io, 0700)
{
  if (!(pcflags & (PC_USER | PC_PUBLIC)) || (pcflags & PC_UIO))
    (*iodisp[((opcode << 4) | ac) & 01777])(opcode, ac, ea);
  else
    i_uuo (opcode, ac, ea);
}

static void
apr_int()
{
  if (aprpiena & aprconi)
    {
      aprconi |= APRIRQ;
      if (aprconi & 07)
	set_interrupt(aprconi, APR_BIT);
    }
  else
    {
      aprconi &= ~APRIRQ;
      if (aprconi & 07)
	clear_interrupt(aprconi, APR_BIT);
    }
}

void
set_nxm(loc, pageinfo)
     addr10 loc;
     unsigned long pageinfo;
{
  aprconi |= APRNXM;
  if (!(pageinfo & PF_WRITEREF))
    aprconi |= APRMBP;		/* NXM gives mem parity error on reads. */

  apr_int();
}

IO_INST(cono,apr,000)
{
  /* XXX Handle I/O reset */

  if ((ea ^ aprconi) & 07)	/* Changing PIA? */
    clear_interrupt (aprconi, APR_BIT);

  if (ea & APRENA)
    aprpiena |= ea & 07760;

  if (ea & APRDIS)
    aprpiena &= ~(ea & 07760);

  if (ea & APRCLR)
    aprconi = (aprconi & 07760 & ~ea) | (ea & 07);
  else if (ea & APRSET)
    aprconi = (aprconi & 07760) | (ea & 07767);
  else
    aprconi = (aprconi & ~07) | (ea & 07); /* Get PIA */

  apr_int();			/* See if interrupt is needed */
}

IO_INST(blki,apr,000)		/* aprid */
{
  word36 tmp;

  dpb (17, 18, 0660401, tmp);	/* TOPS-20 paging, Extended addressing, Ext KL,
				   pmove/pmovem support, ucode version 4.01 */
  dpb (35, 18, 0374066, tmp);	/* Cache, internal channel, PV, Master osc,
				   mca25, serial # 2102. */
  vstore (ea, tmp);
}

IO_INST(coni,apr,000)
{
  word36 tmp;

  dpb(17, 18, aprpiena, tmp);
  dpb(35, 18, aprconi, tmp);

  vstore(ea, tmp);
}

IO_INST(consz,apr,000)
{
  if ((aprconi & ea) == 0)
    incrpc;
}

IO_INST(conso,apr,000)
{
  if (aprconi & ea)
    incrpc;
}

IO_INST(datai,apr,000)
{
  vstore (ea, zero36);		/* Address breaks not implemented */
}

IO_INST(datao,apr,000)		/* Address break */
{
  word36 mem;

  vfetch(ea, mem);

  if (ne36(mem))
    printf ("datao apr non-zero!  %o,,%o\r\n", upper18(mem), lower18(mem));
}

IO_INST(cono,pi,004)
{
  sigset_t oldmask;

  sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

  if (ea & PILEVOFF)
    piconi &= ~(ea & 0177);	/* Turn off selected PI levels */

  if (ea & PILEVON)
    piconi |= ea & 0177;	/* Turn on selected PI levels */

  if (ea & PICLEARSYSTEM)
    {
      ppireq = 0;
      piconi = 0;
    }

  if (ea & PISYSOFF)
    piconi &= ~PISYSON;

  if (ea & PISYSON)
    piconi |= PISYSON;

  if (ea & (PISETINT|PICLEARINT))
    {
      int level;
      int mask;

      mask = ea;
      for (level = 7, mask = ea; level >= 1; level--, mask >>= 1)
	if (mask & 1)
	  if (ea & PISETINT)
	    set_interrupt (level, PI_BIT);
	  else
	    clear_interrupt (level, PI_BIT);

      if (ea & PISETINT)
	ppireq |= ea & 0177;
      else
	ppireq &= ~(ea & 0177);
    }

  if (ea & 0700000)
    {
      printf("Unimplemented PI cono %o at pc %06o\n", ea, pc);
      genunimp();
    }

  calc_interrupt();
  sigprocmask (SIG_SETMASK, &oldmask, NULL);
}

IO_INST(coni,pi,004)
{
  word36 tmp;

  dpb(17, 18, ppireq, tmp);
  dpb(35, 18, piconi, tmp);

  vstore(ea, tmp);
}

IO_INST(consz,pi,004)
{
  if ((piconi & ea) == 0) incrpc;
}

IO_INST(conso,pi,004)
{
  if (piconi & ea) incrpc;
}

IO_INST(blki,pi,004)		/* rdera */
{
  vstore(ea, zero36);
}

IO_INST(blko,pi,004)		/* sbdiag */
{
  word36 mem;

  vfetch(ea, mem);
  ea = increa(ea);		/* XXX */
  vstore(ea, zero36);
}

IO_INST(datai,pag,010)
{
  word36 tmp;

  dpb(17, 18, pagerdataihigh, tmp);
  dpb(35, 18, pagerdatailow, tmp);
  vstore(ea, tmp);
}

IO_INST(datao,pag,010)
{
  word36 mem;
  unsigned int tmp;

  vfetch (ea, mem);
  tmp = upper18 (mem);

  if (tmp & PAGSELECTACBLOCKS)
    {
      currentacblock = acfile + 16 * (tmp >> 9 & 7);
      prevacblock = acfile + 16 * (tmp >> 6 & 7);
      mem_acblock = currentacblock;
      mem_acblock_1 = currentacblock;
      mem_acblock_2 = currentacblock;
      ea_acblock = currentacblock;
      pagerdataihigh = (pagerdataihigh & ~007700) | (tmp & 007700);
    }

  if (tmp & PAGSELECTPCS)
    {
      prev_pcsection = ldb (17, 5, mem) << 18;
      pagerdataihigh = (pagerdataihigh & ~037) | (prev_pcsection >> 18);
    }

  if (tmp & PAGLOADUBA)
    {
      unsigned int uba;

      uba = ldb (35, 13, mem);

      pagerdatailow = (pagerdatailow & ~017777) | uba;
    }

  if (tmp & PAGLOADUBA)
    clear_tlb (tmp & PAGSAVEKEPT);
}

IO_INST(cono,pag,010)
{
  if ((pagerconi ^ ea) & PAGKL)
    {
      if (ea & PAGKL)
	fprintf (stderr, "[kx10: Switching to TOPS-20 paging]\r\n");
      else
	fprintf (stderr, "[kx10: Switching to Tops-10 paging]\r\n");
    }

  pagerconi = ea & HWORDMASK;

  clear_tlb (1);		/* Clear entire TLB (including entries with
				   keep bits). */
}

IO_INST(coni,pag,010)
{
  word36 tmp;

  tmp = zero36;
  dpb(35, 18, pagerconi, tmp);
  vstore(ea, tmp);
}

IO_INST(consz,pag,010)
{
  if ((pagerconi & ea) == 0) incrpc;
}

IO_INST(conso,pag,010)
{
  if (pagerconi & ea) incrpc;
}

IO_INST(blko,pag,010)
{
  clrpt(ea);
}

IO_INST(datai,cca,014)		/* Sweep cache, invalidate all */
{
  aprconi |= APRSWD;		/* Cache sweep done */
  apr_int();
}

IO_INST(cono,tim,020)
{
  if (ea & 020000)
    {
      timconi &= ~030000;
      clear_interrupt (mtrconi, TIMER_BIT);
    }
}

IO_INST(coni,tim,020)
{
  word36 tmp;

  tmp = zero36;

  dpb (35, 18, timconi, tmp);

  vstore(ea, tmp);
}

IO_INST(datai,tim,020)
{
  struct timeval tp;
  word36 mem0, mem1;
  int most, less, rest;

  gettimeofday (&tp, 0);
  tp.tv_usec -= boottime.tv_usec;
  tp.tv_sec -= boottime.tv_sec;
  if (tp.tv_usec < 0)
    {
      tp.tv_usec += 1000000;
      tp.tv_sec--;
    }

  /* This is based on the fact that 2^20 is 1048576,
     so X*1000000 = (X<<20)-(X*48576) */

  rest = tp.tv_sec * 48576;

  /* most significant 36 bits */

  most = (tp.tv_sec >> 3) - (rest >> 23);

  /* less significant 23 bits */

  less = ((tp.tv_sec & 07) << 20) - (rest & 037777777) + tp.tv_usec;

  while (less < 0)		/* correct weird case */
    {
      less += 1 << 23;
      most--;
    }

  mem0 = mem1 = zero36;

  upper18 (mem0) = most >> 18;
  lower18 (mem0) = most & HWORDMASK;

  dpb (23, 23, less, mem1);

  vstore (ea, mem0);
  ea = increa (ea);
  vstore (ea, mem1);
}

IO_INST(blko,tim,020)		/* Perf analysis enables */
{
  word36 mem;

  vfetch(ea, mem);

  if (ne36(mem))
    printf ("Blko tim non-zero!  %o,,%o\r\n", upper18(mem), lower18(mem));
}

IO_INST(coni,mtr,024)
{
  word36 tmp;

  tmp = zero36;			/* XXX Fudge it for now */
  dpb (35, 18, mtrconi, tmp);

  vstore(ea, tmp);
}

static void
tick()
{
  timconi |= 020000;

  set_interrupt(mtrconi, TIMER_BIT);
}

IO_INST(cono,mtr,024)
{
  struct itimerval iv;
  sigset_t oldmask;

  /* If we are changing the PIA, we need to make sure that:
     1) The old PIA is cleared, and
     2) That tick insn't called with PIA == 0.
   */

  /* Turn off interrupts to prevent tick() from running while playing with coni. */
  
  sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

  if ((mtrconi ^ ea) & 7)
    clear_interrupt (mtrconi, TIMER_BIT); /* Clear any existing interrupts */

  mtrconi = ea & HWORDMASK;	/* Update the coni */

  if (mtrconi & 7)
    {
      struct sigaction action;
      sigset_t nullsigmask = {0};

      action.sa_handler = tick;
      action.sa_mask = nullsigmask;
      action.sa_flags = SA_RESTART;
      sigaction (SIGALRM, &action, NULL);

      iv.it_interval.tv_sec = 0;
      iv.it_interval.tv_usec = 1000;
      iv.it_value.tv_sec = 1;
      iv.it_value.tv_usec = 0;

      setitimer (ITIMER_REAL, &iv, NULL);
    }
  else
    {
      signal (SIGALRM, SIG_IGN);

      iv.it_interval.tv_sec = 0;
      iv.it_interval.tv_usec = 0;
      iv.it_value.tv_sec = 0;
      iv.it_value.tv_usec = 0;

      setitimer (ITIMER_REAL, &iv, NULL);
    }
  sigprocmask (SIG_SETMASK, &oldmask, NULL);
}

IO_INST(cono,dte,200)
{
  if ((ea & ~DTETO11DB & ~DTEPIENB & ~DTETO10DB & ~07) & HWORDMASK) {
    printf("Unimplemented DTE cono %o at pc %06o\n", ea, pc);
    genunimp();
  }

  if (ea & DTEPIENB)
    {
      dteconi = (dteconi & ~07) | (ea & 07);
    }

  if (ea & DTETO10DB)		/* Clear To-10 doorbell */
    dteconi &= ~DTETO10DB;

  if (ea & DTETO11DB)		/* Ding dong? */
    dte_doorbell();

/* Now, check for interrupt conditions caused by setting of previous bits,
   and initiate if necessary. */

  if (dteconi & 07)
    {
      if (dteconi & DTETO10DB)
	set_interrupt(dteconi, DTE_BIT);
      else
	clear_interrupt(dteconi, DTE_BIT);
    }
}

IO_INST(consz,dte,200)
{
  if ((dteconi & ea) == 0) incrpc;
}

IO_INST(conso,dte,200)
{
  if (dteconi & ea) incrpc;
}
