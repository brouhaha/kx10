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

static struct dcb *dcbvec[128];

static
IO_INST (cono,generic, 000)
{
  if (dcb->hi_coni_ptr)
    *dcb->hi_coni_ptr = ea;

  if (dcb->lo_coni_ptr)
    *dcb->lo_coni_ptr = ea;
}

static
IO_INST (coni,generic,000)
{
  word36 mem;

  if (dcb->hi_coni_ptr)
    {
      dpb (17, 18, *dcb->hi_coni_ptr, mem);
    }
  else
    {
      dpb (17, 18, 0, mem);
    }

  if (dcb->lo_coni_ptr)
    {
      dpb (35, 18, *dcb->lo_coni_ptr, mem);
    }
  else
    {
      dpb (35, 18, 0, mem);
    }

  vstore (ea, mem);
}

static
IO_INST (conso,generic,000)
{
  int conival;

  conival = dcb->lo_coni_ptr ? *dcb->lo_coni_ptr : 0;

  if (conival & ea)
    incrpc;
}

static
IO_INST (consz,generic,000)
{
  int conival;

  conival = dcb->lo_coni_ptr ? *dcb->lo_coni_ptr : 0;

  if ((conival & ea) == 0)
    incrpc;
}

static
IO_INST (datao,generic,000)
{
  word36 mem;

  vfetch (ea, mem);

  if (dcb->hi_datai_ptr)
    *dcb->hi_datai_ptr = upper18 (mem);

  if (dcb->lo_datai_ptr)
    *dcb->lo_datai_ptr = lower18 (mem);
}

static
IO_INST (datai,generic,000)
{
  word36 mem;

  if (dcb->hi_datai_ptr)
    {
      dpb (17, 18, *dcb->hi_datai_ptr, mem);
    }
  else
    {
      dpb (17, 18, 0, mem);
    }

  if (dcb->lo_datai_ptr)
    {
      dpb (35, 18, *dcb->lo_datai_ptr, mem);
    }
  else
    {
      dpb (35, 18, 0, mem);
    }

  vstore (ea, mem);
}

static
IO_INST (blko,generic,000)
{
  word36 mem;
  addr10 addr;
  int count;

  vfetch (ea, mem);		/* Get pointer */

  count = upper18 (mem) + 1;
  addr = pcsection | ((lower18 (mem) + 1) & HWORDMASK);

  (*dcb->funcs)[3] (dcb, dev, opcode, addr);

  if (count != 01000000)	/* Count overflow? */
    incrpc;			/* No, skip instruction */

  dpb (17, 18, count, mem);
  dpb (35, 18, addr, mem);

  vstore (ea, mem);
}

static
IO_INST (blki,generic,000)
{
  word36 mem;
  addr10 addr;
  int count;

  vfetch (ea, mem);		/* Get pointer */

  count = upper18 (mem) + 1;
  addr = pcsection | ((lower18 (mem) + 1) & HWORDMASK);

  (*dcb->funcs)[1] (dcb, dev, opcode, addr);

  if (count != 01000000)	/* Count overflow? */
    incrpc;			/* No, skip instruction */

  dpb (17, 18, count, mem);
  dpb (35, 18, addr, mem);

  vstore (ea, mem);
}

/* Unimplimented I/O insns come here.  We convert the device number and opcode
   back into a form that i_unimp can deal with.  */

IO_INST (unimp,unknown,000)
{
  i_unimp (0700 | (dev >> 1), ((dev << 3) | opcode) & 017, ea);
}

void
define_device (devno, hi_coni_ptr, lo_coni_ptr, hi_datai_ptr, lo_datai_ptr,
	       cono_rtn, datao_rtn)
     int devno, *hi_coni_ptr, *lo_coni_ptr, *hi_datai_ptr, *lo_datai_ptr;
     void (*cono_rtn) PARAMS ((struct dcb *dcb, addr10 ea));
     void (*datao_rtn) PARAMS ((struct dcb *dcb, addr10 ea));
{
  struct dcb *dcb;
}

static io_func_type *badfuncs[8] =
{
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
};

/* This is for unknown devices.  Using them will generate errors.  */

static
struct dcb baddcb =
{
  NULL, NULL, NULL, NULL,
  &badfuncs
};

static io_func_type *unimpfuncs[8] =
{
  i_blki_generic,
  i_datai_generic,
  i_blko_generic,
  i_datao_generic,
  i_cono_generic,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

/* This is for unimplemented devices.  Noops for the most part.  */

static
struct dcb unimpdcb =
{
  NULL, NULL, NULL, NULL,
  &unimpfuncs
};

static io_func_type i_cono_apr;
static io_func_type i_datao_apr;
static io_func_type i_blki_apr;

static io_func_type *aprfuncs[8] =
{
  i_blki_apr,
  i_datai_generic,
  i_unimp_unknown,
  i_datao_apr,
  i_cono_apr,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

static
struct dcb aprdcb =
{
  &aprpiena, &aprconi, NULL, NULL,
  &aprfuncs
};

static io_func_type i_cono_pi;
static io_func_type i_blko_pi;

static io_func_type *pifuncs[8] =
{
  i_datai_generic,		/* blki pi, = rdera (returns zero) */
  i_unimp_unknown,		/* datai pi, */
  i_blko_pi,			/* sbdiag */
  i_unimp_unknown,		/* datao pi, */
  i_cono_pi,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

static
struct dcb pidcb =
{
  &ppireq, &piconi, NULL, NULL,
  &pifuncs
};

static io_func_type i_cono_pag;
static io_func_type i_datao_pag;
static io_func_type i_blko_pag;

static io_func_type *pagfuncs[8] =
{
  i_unimp_unknown,		/* blki pag, */
  i_datai_generic,
  i_blko_pag,			/* clrpt */
  i_datao_pag,
  i_cono_pag,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

static
struct dcb pagdcb =
{
  NULL, &pagerconi, &pagerdataihigh, &pagerdatailow,
  &pagfuncs
};

static io_func_type i_datai_cca;

static io_func_type *ccafuncs[8] =
{
  i_unimp_unknown,
  i_datai_cca,			/* Sweep cache */
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
};

static
struct dcb ccadcb =		/* Cache controller */
{
  NULL, NULL, NULL, NULL,
  &ccafuncs
};

static io_func_type i_cono_tim;
static io_func_type i_datai_tim;
static io_func_type i_blko_tim;

static io_func_type *timfuncs[8] =
{
  i_unimp_unknown,		/* blki tim, */
  i_datai_tim,			/* rdtime */
  i_blko_tim,			/* perf analysis */
  i_unimp_unknown,		/* datao tim, */
  i_cono_tim,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

static
struct dcb timdcb =
{
  NULL, &timconi, NULL, NULL,
  &timfuncs
};

static io_func_type i_cono_mtr;

static io_func_type *mtrfuncs[8] =
{
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_unimp_unknown,
  i_cono_mtr,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

static
struct dcb mtrdcb =
{
  NULL, &mtrconi, NULL, NULL,
  &mtrfuncs
};

static io_func_type i_cono_dte;

static io_func_type *dtefuncs[8] =
{
  i_blki_generic,
  i_datai_generic,
  i_blko_generic,
  i_datao_generic,
  i_cono_dte,
  i_coni_generic,
  i_consz_generic,
  i_conso_generic,
};

static
struct dcb dtedcb =
{
  NULL, &dteconi, NULL, NULL,
  &dtefuncs
};

void
io_init ()
{
  extern int tops10_paging;
  extern struct dcb rh0dcb, knidcb;
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

  for (i = 0; i <= 127; i++)
    dcbvec[i] = &baddcb;

  dcbvec[000 >> 2] = &aprdcb;
  dcbvec[004 >> 2] = &pidcb;
  dcbvec[010 >> 2] = &pagdcb;
  dcbvec[014 >> 2] = &ccadcb;
  dcbvec[020 >> 2] = &timdcb;
  dcbvec[024 >> 2] = &mtrdcb;
  dcbvec[0200 >> 2] = &dtedcb;
  dcbvec[0540 >> 2] = &rh0dcb;
  dcbvec[0564 >> 2] = &knidcb;

  dcbvec[0110 >> 2] = &unimpdcb; /* cdp - Card punch */
  dcbvec[0120 >> 2] = &unimpdcb; /* tty - KA console */
  dcbvec[0204 >> 2] = &unimpdcb; /* dte1 - DTE1 */
  dcbvec[0210 >> 2] = &unimpdcb; /* dte2 - DTE2 */
  dcbvec[0214 >> 2] = &unimpdcb; /* dte3 - DTE3 */
  dcbvec[0270 >> 2] = &unimpdcb; /* rmc - RH10 # 0 */
  dcbvec[0274 >> 2] = &unimpdcb; /* rmc2 - RH10 # 1 */
  dcbvec[0354 >> 2] = &unimpdcb; /* tms2 - TM10 */
  dcbvec[0360 >> 2] = &unimpdcb; /* rmc3 - RH10 # 2 */
  dcbvec[0364 >> 2] = &unimpdcb; /* rmc4 - RH10 # 3 */
  dcbvec[0370 >> 2] = &unimpdcb; /* rmc5 - RH10 # 4 */
  dcbvec[0374 >> 2] = &unimpdcb; /* rmc6 - RH10 # 5 */
  dcbvec[0404 >> 2] = &unimpdcb; /* waits - strange WAITS device */
  dcbvec[0520 >> 2] = &unimpdcb; /* an0 - AN20#0 */
  dcbvec[0524 >> 2] = &unimpdcb; /* an1 - AN20#1 */
  dcbvec[0530 >> 2] = &unimpdcb; /* waits1 - another strange WAITS device */
  dcbvec[0544 >> 2] = &unimpdcb; /* rh1 - RH20 # 1 */
  dcbvec[0550 >> 2] = &unimpdcb; /* rh2 - RH20 # 2 */
  dcbvec[0554 >> 2] = &unimpdcb; /* rh3 - RH20 # 3 */
  dcbvec[0560 >> 2] = &unimpdcb; /* rh4 - RH20 # 4 */
				/* RH20 # 5 KNI */
  dcbvec[0570 >> 2] = &unimpdcb; /* rh6 - RH20 # 6 */
  dcbvec[0574 >> 2] = &unimpdcb; /* rh7 - RH20 # 7 */

  dcbvec[0774 >> 2] = &unimpdcb; /* unk - ?!?! */
}

/* Handle blki/blko to nonexistent device.  We go through the motions, but
   output data goes nowhere, and input data is always zero.  */

#if 0
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
#endif

static void (*iodisp[02000]) PARAMS ((addr10 ea));

/* Define a non-existent device to prevent kx10 traps.  */

static void
no_device (device)
     int device;
{
  extern void i_setzm(), i_trn(), i_trna();

  if (device & 03)
    abort ();

  device <<= 1;			/* Make room for opcode */

/*  iodisp[device + 0] = i_blkio_nodev;	/* BLKI */
  iodisp[device + 1] = i_setzm;		/* DATAI */
/*  iodisp[device + 2] = i_blkio_nodev;	/* BLKO */
  iodisp[device + 3] = i_trn;		/* DATAO */
  iodisp[device + 4] = i_trn;		/* CONO */
  iodisp[device + 5] = i_setzm;		/* CONI */
  iodisp[device + 6] = i_trna;		/* CONSZ */
  iodisp[device + 7] = i_trn;		/* CONSO */
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

#if 0
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
#endif

IO_INST(datao,apr,000)		/* Address break */
{
  word36 mem;

  vfetch (ea, mem);

  if (ne36 (mem))
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

#if 0
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
#endif

IO_INST(blko,pi,004)		/* sbdiag */
{
  word36 mem;

  vfetch (ea, mem);
  ea = increa(ea);		/* XXX */
  vstore (ea, zero36);
}

#if 0
IO_INST(datai,pag,010)
{
  word36 tmp;

  dpb(17, 18, pagerdataihigh, tmp);
  dpb(35, 18, pagerdatailow, tmp);
  vstore(ea, tmp);
}
#endif

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

#if 0
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
#endif

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

#if 0
IO_INST(coni,tim,020)
{
  word36 tmp;

  tmp = zero36;

  dpb (35, 18, timconi, tmp);

  vstore(ea, tmp);
}

IO_INST(consz,tim,020)
{
  if ((timconi & ea) == 0)
    incrpc;
}

IO_INST(conso,tim,020)
{
  if (timconi & ea)
    incrpc;
}
#endif

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

#if 0
IO_INST(coni,mtr,024)
{
  word36 tmp;

  tmp = zero36;			/* XXX Fudge it for now */
  dpb (35, 18, mtrconi, tmp);

  vstore(ea, tmp);
}
#endif

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

#if 0
IO_INST(consz,dte,200)
{
  if ((dteconi & ea) == 0) incrpc;
}

IO_INST(conso,dte,200)
{
  if (dteconi & ea) incrpc;
}
#endif

/* All I/O instructions come through here.  This does the second level dispatch
   through a switch table.  */

static inline
SPECIAL_INST (io, 000)
{
  int dev;
  int op;
  struct dcb *dcb;

  dev = (opcode << 1 | ac >> 3) & 0177;
  dcb = dcbvec[dev];
  op = ac & 07;

  if (!(pcflags & (PC_USER | PC_PUBLIC)) || (pcflags & PC_UIO))
    (*dcb->funcs)[op] (dcb, dev, op, ea);
  else
    i_uuo (opcode, ac, ea);
}

INST (io700, 0700)
{
  i_io (0700, ac, ea);
}

INST (io701, 0701)
{
  i_io (0701, ac, ea);
}

INST (io702, 0702)
{
  i_io (0702, ac, ea);
}

INST (io703, 0703)
{
  i_io (0703, ac, ea);
}

INST (io704, 0704)
{
  i_io (0704, ac, ea);
}

INST (io705, 0705)
{
  i_io (0705, ac, ea);
}

INST (io706, 0706)
{
  i_io (0706, ac, ea);
}

INST (io707, 0707)
{
  i_io (0707, ac, ea);
}

INST (io710, 0710)
{
  i_io (0710, ac, ea);
}

INST (io711, 0711)
{
  i_io (0711, ac, ea);
}

INST (io712, 0712)
{
  i_io (0712, ac, ea);
}

INST (io713, 0713)
{
  i_io (0713, ac, ea);
}

INST (io714, 0714)
{
  i_io (0714, ac, ea);
}

INST (io715, 0715)
{
  i_io (0715, ac, ea);
}

INST (io716, 0716)
{
  i_io (0716, ac, ea);
}

INST (io717, 0717)
{
  i_io (0717, ac, ea);
}

INST (io720, 0720)
{
  i_io (0720, ac, ea);
}

INST (io721, 0721)
{
  i_io (0721, ac, ea);
}

INST (io722, 0722)
{
  i_io (0722, ac, ea);
}

INST (io723, 0723)
{
  i_io (0723, ac, ea);
}

INST (io724, 0724)
{
  i_io (0724, ac, ea);
}

INST (io725, 0725)
{
  i_io (0725, ac, ea);
}

INST (io726, 0726)
{
  i_io (0726, ac, ea);
}

INST (io727, 0727)
{
  i_io (0727, ac, ea);
}

INST (io730, 0730)
{
  i_io (0730, ac, ea);
}

INST (io731, 0731)
{
  i_io (0731, ac, ea);
}

INST (io732, 0732)
{
  i_io (0732, ac, ea);
}

INST (io733, 0733)
{
  i_io (0733, ac, ea);
}

INST (io734, 0734)
{
  i_io (0734, ac, ea);
}

INST (io735, 0735)
{
  i_io (0735, ac, ea);
}

INST (io736, 0736)
{
  i_io (0736, ac, ea);
}

INST (io737, 0737)
{
  i_io (0737, ac, ea);
}

INST (io740, 0740)
{
  i_io (0740, ac, ea);
}

INST (io741, 0741)
{
  i_io (0741, ac, ea);
}

INST (io742, 0742)
{
  i_io (0742, ac, ea);
}

INST (io743, 0743)
{
  i_io (0743, ac, ea);
}

INST (io744, 0744)
{
  i_io (0744, ac, ea);
}

INST (io745, 0745)
{
  i_io (0745, ac, ea);
}

INST (io746, 0746)
{
  i_io (0746, ac, ea);
}

INST (io747, 0747)
{
  i_io (0747, ac, ea);
}

INST (io750, 0750)
{
  i_io (0750, ac, ea);
}

INST (io751, 0751)
{
  i_io (0751, ac, ea);
}

INST (io752, 0752)
{
  i_io (0752, ac, ea);
}

INST (io753, 0753)
{
  i_io (0753, ac, ea);
}

INST (io754, 0754)
{
  i_io (0754, ac, ea);
}

INST (io755, 0755)
{
  i_io (0755, ac, ea);
}

INST (io756, 0756)
{
  i_io (0756, ac, ea);
}

INST (io757, 0757)
{
  i_io (0757, ac, ea);
}

INST (io760, 0760)
{
  i_io (0760, ac, ea);
}

INST (io761, 0761)
{
  i_io (0761, ac, ea);
}

INST (io762, 0762)
{
  i_io (0762, ac, ea);
}

INST (io763, 0763)
{
  i_io (0763, ac, ea);
}

INST (io764, 0764)
{
  i_io (0764, ac, ea);
}

INST (io765, 0765)
{
  i_io (0765, ac, ea);
}

INST (io766, 0766)
{
  i_io (0766, ac, ea);
}

INST (io767, 0767)
{
  i_io (0767, ac, ea);
}

INST (io770, 0770)
{
  i_io (0770, ac, ea);
}

INST (io771, 0771)
{
  i_io (0771, ac, ea);
}

INST (io772, 0772)
{
  i_io (0772, ac, ea);
}

INST (io773, 0773)
{
  i_io (0773, ac, ea);
}

INST (io774, 0774)
{
  i_io (0774, ac, ea);
}

INST (io775, 0775)
{
  i_io (0775, ac, ea);
}

INST (io776, 0776)
{
  i_io (0776, ac, ea);
}

INST (io777, 0777)
{
  i_io (0777, ac, ea);
}
