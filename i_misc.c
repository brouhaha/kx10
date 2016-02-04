/* Miscellaneous instructions for kx10, the PDP-10 emulator.
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
extern void (*opdisp[01000])();

void
genunimp ()
{
  console_reset();
#ifdef PCHIST
  ppc();
#endif /* PCHIST */
  suspend();
  console_init();

  return;
}

SPECIAL_INST(unimp, 000)
{
  static char *iocodes[] = {"blki", "datai", "blko", "datao", "cono",
			      "coni", "consz", "conso"};
  static char *iodevs[] = {"apr", "pi", "pag", "cca", "tim", "mtr"};
  static char *extends[] = {"0", "cmpsl", "cmpse", "cmpsle", "edit", "cmpsge",
			      "cmpsn", "cmpsg",
			      "cvtdbo", "cvtdbt", "cvtbdo", "cvsbdt",
			      "movso", "movst", "movslj", "movsrj",
			      "xblt",
			      "gsngl", "gdble", "gdfix", "gfix", "gdfixr",
			      "gfixr", "dgfltr", "gfltr", "gfsc"};
  extern char *opnames[];
  int dev;

  printf("\r\nUnimplemented instruction at %o: ", pcsection|(pc-1));

  if (opcode >= 0700)
    {				/* I/O instruction */
      dev = ((opcode << 1) | (ac >> 3)) & 0177;

      printf("%s ", iocodes[ac & 7]);

      if (dev > 5)
	printf("%o", dev << 2);
      else
	printf("%s", iodevs[dev]);
    }
  else
    printf("%s %o", opnames[opcode], ac);

  printf (", %o\r\n", ea);

  if (opcode == 0123)		/* Extend? */
    {
      word36 tmp;
      int extop;

      vfetch (ea, tmp);
      extop = ldb (8, 9, tmp);

      printf("Extend opcode: ");
      if (extop < sizeof extends / sizeof (char *))
	printf ("%s\r\n", extends[extop]);
      else
	printf ("0%o\r\n", extop);
    }

  printf ("Flags: %06o,,000000\r\n", pcflags << 5);

  if (pcflags & PC_USER)
    {
      void i_uuo ();

      i_uuo (opcode, ac, ea);
      return;
    }

  genunimp();
}

SPECIAL_INST(uuo, 0)
{
  word36 tmp;
  addr10 vec;

  if (!(pagerconi & PAGKL))
    {
      vfetch (pc - 1, tmp);
      ustore (0424, tmp);	/* Store UUO */

      dpb (17, 18, pcflags << 5, tmp);
      dpb (35, 18, pc, tmp);
      ustore (0425, tmp);	/* PC & flags */

      dpb (17, 18, pagerdataihigh, tmp);
      dpb (35, 18, pagerdatailow, tmp);
      ustore (0426, tmp);	/* Process context word */
    }
  else
    {
      dpb (17, 18, pcflags << 5, tmp);
      dpb (35, 18, (opcode << 9) | (ac << 5) | (prev_pcsection >> 18), tmp);
      ustore (0424, tmp);

      dpb (17, 18, pcsection >> 18, tmp);
      dpb (35, 18, pc, tmp);
      ustore (0425, tmp);

      dpb (5, 6, 0, tmp);
      dpb (35, 30, ea, tmp);
      ustore (0426, tmp);

      dpb (17, 18, pagerdataihigh, tmp);
      dpb (35, 18, pagerdatailow, tmp);
      ustore (0427, tmp);
    }

  if (trapflags & (PC_TRAP1 | PC_TRAP2))
    vec = 0431;
  else
    vec = 0430;

  if (pcflags & PC_PUBLIC)
    vec += 2;

  if (pcflags & PC_USER)
    {
      pcflags = PC_PCU;
      vec += 4;
      goexec ();
    }
  else
    pcflags = 0;

  prev_pcsection = pcsection;
  pagerdataihigh = (pagerdataihigh & ~037) | (prev_pcsection >> 18);

  ufetch (vec, &tmp);

  if (!(pagerconi & PAGKL))
    {
      pcflags = ldb (12, 13, tmp);
      if (pcflags & PC_USER)
	gouser ();
      setpc (lower18 (tmp));
    }
  else
    setpc (ldb (35, 30, tmp));
}

SPECIAL_INST(luuo, 001)
{
  word36 tmp;
  word36 ir;

  if (pcsection != 0)
    {
      if (pcflags & PC_USER)
	{
	  addr10 luuoblock;

	  ufetch (0420, &tmp);

	  luuoblock = ldb (35, 30, tmp);

	  dpb (17, 18, pcflags << 5, tmp);
	  dpb (35, 18, (opcode << 9) | (ac << 5), tmp);
	  ustore (luuoblock + 0, tmp);

	  dpb (17, 18, pcsection >> 18, tmp);
	  dpb (35, 18, pc, tmp);
	  ustore (luuoblock + 1, tmp);

	  dpb (5, 6, 0, tmp);
	  dpb (35, 30, ea, tmp);
	  ustore (luuoblock + 2, tmp);

	  ufetch (luuoblock + 3, &tmp);
	  setpc (ldb (35, 30, tmp));
	  return;
	}
      else
	{
	  i_uuo (opcode, ac, ea);
	  return;
	}
    }

  dpb (17, 18, (opcode << 9) | (ac << 5), tmp);
  dpb (35, 18, ea, tmp);
  vstore (040, tmp);

  vfetch (041, ir);
  opcode = ldb (8, 9, ir);
  ac = ldb (12, 4, ir);
  ea = ieacalc(ir, ea&SECTMASK); /* Calc ea in the section of the inst */
  (*opdisp[opcode])(opcode, ac, ea); /* Recursively call the interpreter */
}

INST(pmove, 052)
{
  word36 tmp;

#if 1
  if (!(pcflags & (PC_USER | PC_PUBLIC)) || (pcflags & PC_UIO))
    {
      vfetch (ea, tmp);		/* Fetch physical address */

      pfetch (ldb (35, 22, tmp), AC); /* Fetch contents */
    }
  else
#endif
    i_uuo (opcode, ac, ea);
}

INST(pmovem, 053)
{
  word36 tmp;

#if 1
  if (!(pcflags & (PC_USER | PC_PUBLIC)) || (pcflags & PC_UIO))
    {
      vfetch (ea, tmp);		/* Fetch physical address */

      pstore (ldb (35, 22, tmp), AC); /* Store contents */
    }
  else
#endif
    i_uuo (opcode, ac, ea);
}

INST(uuo101, 0101)
{
  i_uuo (opcode, ac, ea);
}

INST(extend, 0123)
{
  word36 tmp;

  vfetch (ea, tmp);

  opcode = ldb (8, 9, tmp);

  switch (opcode)
    {
    case 000:			/* illegal */
      i_uuo (0123, ac, ea);
      return;
    case 001:			/* cmpsl */
    case 002:			/* cmpse */
    case 003:			/* cmpsle */
    case 005:			/* cmpsge */
    case 006:			/* cmpsn */
    case 007:			/* cmpsg */
      compare_strings (opcode, ac, ea);
      return;
#if 0
    case 04:			/* edit */
      edit (opcode, ac, ea, tmp);
      return;
    case 010:			/* cvtdbo */
      cvtdbo (opcode, ac, ea, tmp);
      return;
    case 011:			/* cvtdbt */
      cvtdbt (opcode, ac, ea, tmp);
      return;
    case 012:			/* cvtbdo */
      cvtbdo (opcode, ac, ea, tmp);
      return;
    case 013:			/* cvtbdt */
      cvtbdt (opcode, ac, ea, tmp);
      return;
#else
    case 04:			/* edit */
    case 010:			/* cvtdbo */
    case 011:			/* cvtdbt */
    case 013:			/* cvtbdt */
      i_unimp (0123, ac, ea);
      return;
#endif
    case 012:			/* cvtbdo */
      /* CVTBDO Kludge to make EDDT happy!!! */
      dpb(1, 1, 1, ACplus3);	/* Set the N bit to make EDDT think that we are a KL */
      return;
    case 014:			/* movso */
    case 015:			/* movst */
    case 016:			/* movslj */
    case 017:			/* movsrj */
      move_string (opcode, ac, ea, tmp);
      return;
    case 020:			/* xblt */
      {
	int length;
	addr10 src, dest;
	word36 tmp;

	length = ldb(35, 32, AC);
	src = ldb(35, 30, ACplus1) | GLOBAL;
	dest = ldb(35, 30, ACplus2) | GLOBAL;

	if (length < 0)
	  {
	    /* This is very inefficient if we get a page fault in the middle of a big
	       blt, as we restart the blt from the beginning. */

	    for (; length < 0; length++)
	      {
		src--;
		dest--;

		vfetch_2(src, tmp);
		vstore_1(dest, tmp);

		incr36 (AC);
		decr36 (ACplus1);
		decr36 (ACplus2);
	      }
	  }
	else
	  {
	    /* This is very inefficient if we get a page fault in the middle of a big
	       blt, as we restart the blt from the beginning. */

	    for (; length > 0; length--, src++, dest++)
	      {
		vfetch_2(src, tmp);
		vstore_1(dest, tmp);

		decr36 (AC);
		incr36 (ACplus1);
		incr36 (ACplus2);
	      }
	  }
      }
      return;
    default:
      i_uuo (0123, ac, ea);
      return;
    }
}

INST(jffo, 0243)
{
  int bit;

  if (eq36(AC))
    {
      ACplus1 = zero36;
      return;
    }
  
  for (bit = 0; bit <= 35; bit++)
    if (biton(bit, AC))
      break;

  dpb (17, 18, 0, ACplus1);
  dpb (35, 18, bit, ACplus1);

  setpc (ea);
}

INST(uuo247, 0247)
{
  i_uuo (opcode, ac, ea);
}

INST(blt, 0251)
{
  word36 tmp;
  addr10 source, dest;

  source = upper18(AC);
  dest = lower18(AC);

/* Add section to source and dest addresses */

#if 1
  source |= ea & (GLOBAL|SECTMASK);
#else
  source |= pcsection;
#endif
  dest |= ea & (GLOBAL|SECTMASK);

  /* This is a crock to make Waits work.  It depends upon this to determine
     that it's running on a KL. */

  if (source == dest
      && source == ac)
    {
      AC = zero36;
    }

#if 0
  {
    int len;
    addr10 srcbeg, srclst, dstbeg, dstlst;
    word36 mem;

    vfetch_1 (source, mem);

    srcbeg = source & ~GLOBAL;
    dstbeg = dest & ~GLOBAL;
    dstlst = ea & ~GLOBAL;

    len = dstlst - dstbeg;
    srclst = srcbeg + len;

    if ((srcbeg != dstbeg || srclst != dstlst)
	&& (srcbeg + 1 != dstbeg
	    || ne36(mem)))
      {
	if ((srcbeg >= dstbeg && srcbeg <= dstlst)
	    || (srclst >= dstbeg && srclst <= dstlst))
	  printf ("Overlapping BLT at pc %o!  src = 0%o=>0%o dst = o%o=>0%o\r\n",
		  pcsection | pc, srcbeg, srclst, dstbeg, dstlst);
      }
  }
#endif

  /* This is very inefficient if we get a page fault in the middle of a big
     blt, as we restart the blt from the beginning. */

  while (1)
    {
      vfetch_1 (source, tmp);
      vstore (dest, tmp);
      source = increa (source);
      dest = increa (dest);

      if (dest > ea)
	break;

      dpb (17, 18, source, AC);
      dpb (35, 18, dest, AC);
    }

  if (((dest - 1) & HWORDMASK) == ac)
    return;

  dpb(17, 18, source, AC);
  dpb(35, 18, dest, AC);
}

INST(jrst, 0254)
{
  switch (ac) {
  case 01:			/* Portal.  For now, it just acts like jrst */
    {
      word36 tmp;

      tmp = map (ea);

      if (biton (6, tmp))	/* Check public bit in map */
	pcflags |= PC_PUBLIC;
      else
	pcflags &= ~PC_PUBLIC;
    }
  case 00:			/* Normal jrst */
    setpc(ea);
    return;
  case 02:			/* jrstf */
/* jrstf is one of the ugliest instructions in the whole machine.  Doing it
 * 'right' would mean that I would have to get the eacalc routine to save
 * the last word fetched from either an AC or mem.  This would slow down the
 * eacalc, so I refuse to put it there.  Instead, I have implemented a hacked
 * eacalc that can return the right stuff just for jrstf...
 */
    {
      int jrstfflags;
      word36 ir;

      vfetch (pc - 1, ir);	/* XXX fix this hack... */
      ea = jrstfeacalc (ir, pcsection, &jrstfflags);
      if (pcflags & PC_USER)
	pcflags = jrstfflags | PC_USER;
      else
	{
	  pcflags = jrstfflags;
	  if (pcflags & PC_USER)
	    gouser ();
	}
      setpc (ea);
      if (pcflags & (PC_TRAP1 | PC_TRAP2))
	{
	  word36 foo;

	  vfetch_i (pcsection | pc, foo);
	  setting_pc = 1;
	  setflags (0);
	  setting_pc = 0;
	}
    }
    return;
  case 012:			/* jen */
/* jrstf/jen is one of the ugliest instructions in the whole machine.  Doing it
 * 'right' would mean that I would have to get the eacalc routine to save
 * the last word fetched from either an AC or mem.  This would slow down the
 * eacalc, so I refuse to put it there.  Instead, I have implemented a hacked
 * eacalc that can return the right stuff just for jrstf...
 */
    {
      int jrstfflags;
      word36 ir;

      if (pcflags & (PC_USER | PC_PUBLIC)) /* User mode? */
	{
	  i_uuo (opcode, ac, ea);	/* Yes, bomb him out */
	  return;
	}

      vfetch (pc - 1, ir);	/* XXX fix this hack... */
      ea = jrstfeacalc (ir, pcsection, &jrstfflags);

      pcflags = jrstfflags;

      if (pcflags & PC_USER)
	gouser ();

      setpc (ea);
      finish_interrupt ();
      if (pcflags & (PC_TRAP1 | PC_TRAP2))
	{
	  word36 foo;

	  vfetch_i (pcsection | pc, foo);
	  setting_pc = 1;
	  setflags (0);
	  setting_pc = 0;
	}
    }
    return;
  case 04:			/* Halt */
    if (pcflags & (PC_USER | PC_PUBLIC))
      {
#ifdef PCHIST
	ppc ();
#endif
	i_uuo (opcode, ac, ea);
	return;
      }

    printf ("?\007\007Processor executed halt instruction at %0o, new PC is %0o\r\n",
	    pcsection|(pc-1), ea);
    printf ("coni pi = %o,,%o, interrupt = %o\r\n", ppireq, piconi, interrupt);
    setpc (ea);
    genunimp ();
    return;
  case 05:			/* xjrstf */
    {
      word36 flags, newpc;

      vfetch (ea, flags);

      ea = increa (ea);
      vfetch (ea, newpc);
      setpc (ldb (35, 30, newpc));

      if (pcflags & PC_USER)
	pcflags = ldb (12, 13, flags) | PC_USER;
      else
	{
	  prev_pcsection = ldb (35, 5, flags) << 18;
	  pagerdataihigh = (pagerdataihigh & ~037) | (prev_pcsection >> 18);
	  pcflags = ldb (12, 13, flags);
	  if (pcflags & PC_USER)
	    gouser ();
	}
      if (pcflags & (PC_TRAP1 | PC_TRAP2))
	{
	  word36 foo;

	  vfetch_i (pcsection | pc, foo);
	  setting_pc = 1;
	  setflags (0);
	  setting_pc = 0;
	}
    }
    return;
  case 06:			/* xjen */
    {
      word36 flags, newpc;

      if (pcflags & PC_USER)	/* User mode? */
	{
	  i_uuo (opcode, ac, ea); /* Yes, privileged instruction, bomb */
	  return;
	}

      vfetch (ea, flags);

      ea = increa (ea);
      vfetch (ea, newpc);
      setpc (ldb (35, 30, newpc));

      prev_pcsection = ldb (35, 5, flags) << 18;
      pagerdataihigh = (pagerdataihigh & ~037) | (prev_pcsection >> 18);
      pcflags = ldb (12, 13, flags);
      if (pcflags & PC_USER)
	gouser ();

      if (pcflags & (PC_TRAP1 | PC_TRAP2))
	{
	  word36 foo;

	  vfetch_i (pcsection | pc, foo);
	  setting_pc = 1;
	  setflags (0);
	  setting_pc = 0;
	}

      finish_interrupt();	/* Clear current interrupt level */
    }
    return;
  case 07:			/* xpcw */
    {
      word36 flags, tmppc;

      tmppc = zero36;
      dpb (17, 18, pcflags << 5, flags);
      dpb (35, 18, prev_pcsection >> 18, flags);
      vstore (ea, flags);

      ea = increa (ea);
      dpb (35, 30, pcsection | pc, tmppc);
      vstore (ea, tmppc);

      ea = increa (ea);
      vfetch (ea, flags);

      ea = increa (ea);
      vfetch (ea, tmppc);

      if (!(pcflags & PC_USER))	/* Exec mode? */
	{
	  prev_pcsection = ldb (35, 5, flags) << 18;
	  pagerdataihigh = (pagerdataihigh & ~037) | (prev_pcsection >> 18);
	  pcflags = ldb (12, 13, flags);
	}
      else
	pcflags = ldb (12, 13, flags) | PC_USER;

      setpc (ldb (35, 30, tmppc));
      if (pcflags & (PC_TRAP1 | PC_TRAP2))
	{
	  word36 foo;

	  vfetch_i (pcsection | pc, foo);
	  setting_pc = 1;
	  setflags (0);
	  setting_pc = 0;
	}
    }
    return;
  case 010:			/* Just dismiss interrupt */
    if (pcflags & (PC_USER | PC_PUBLIC)) /* User mode? */
      i_uuo (opcode, ac, ea);	/* Yes, privileged instruction, bomb */
    else
      finish_interrupt();	/* Clear current interrupt level */
    return;
  case 014:			/* sfm */
    {
      word36 tmp;

      dpb(17, 18, pcflags << 5, tmp);
      dpb(35, 18, prev_pcsection >> 18, tmp);
      vstore(ea, tmp);
    }
    return;
  case 015:			/* xjrst */
    {
      word36 mem;

      vfetch(ea, mem);
      setpc(ldb(35, 30, mem));
    }
    return;
  default:
    i_unimp (0254, ac, ea);
    return;
  }
}

INST(jfcl, 0255)
{
  if ((ac << 9) & pcflags)
    {
      pcflags &= ~(ac << 9);
      setpc (ea);
    }
}

/*void interpret();*/

INST(xct, 0256)
{
  word36 ir;
  int func;
  extern int saved_pcsection;
  extern trace_ubr;

  func = ac;

  while (1)
    {
      vfetch (ea, ir);		/* Fetch the instruction */
      opcode = ldb (8, 9, ir);
      if (opcode != 0256)	/* Is it an xct? */
	break;			/* No, no problem */

      /* We've got an xct of an xct.  See if an interrupt has
	 occurred, otherwise, loop back for the next xct.  */

      if (interrupt < 0)	/* Interrupt pending? */
	{			/* No, keep on rolling along */
	  func = ldb (12, 4, ir);

	  ea = ieacalc (ir, ea & SECTMASK);
	  continue;
	}

      pc = (pc - 1) & HWORDMASK; /* Continue at this instruction */
      siglongjmp(to_main, 1);	/* Handle the interrupt */
    }

  ac = ldb (12, 4, ir);

#ifdef PROCESS_TRACING
  if (pagerdatailow == trace_ubr
      && pcflags & PC_USER)
    {
      printf ("  %o: ", pcsection | pc);
      print_instruction (ir);
    }
#endif

  if (func == 0 || pcflags & PC_USER) /* Normal xct or user mode */
    {
      ea = ieacalc(ir, ea&SECTMASK); /* Calc ea in the section of the inst */
      (*opdisp[opcode])(opcode, ac, ea); /* Recursively call the interpreter */
      return;
    }

  if (func & 010)
    set_previous_eacalc ();

#if 0
  if (opcode == 0123 || opcode == 0251)	/* For some reason, extend works differently */
    ea = ieacalc(ir, ea & SECTMASK);
  else
    ea = ieacalc(ir, prev_pcsection); /* Calc ea in current context, but with
					 PCS.  This is needed to emulate a KL bug
					 which the monitor depends on.  */
#endif

  if (func & 014)
    ea = ieacalc(ir, prev_pcsection); /* Calc ea in current context, but with
					 PCS.  This is needed to emulate a KL bug
					 which the monitor depends on.  */
  else
    ea = ieacalc(ir, ea & SECTMASK);

  saved_pcsection = pcsection;
  if (func & 03)
    pcsection = prev_pcsection;

  set_previous_context_func (func);

  (*opdisp[opcode])(opcode, ac, ea);

  pcsection = saved_pcsection;
  saved_pcsection = -1;

  clear_previous_context ();
}

INST(map, 0257)
{
  AC = map(ea);
}

INST(jsr, 0264)
{
  word36 tmp;

  tmp = zero36;

  if (pcsection)
    {
      dpb(35, 30, (pcsection | pc), tmp) /* 30 bit pc for non-zero sections */
    }
  else
    {
      dpb(12, 13, pcflags, tmp);	/* PC and flags for sect 0 */
      dpb(35, 18, pc, tmp);
    }

  vstore(ea, tmp);		/* Store current PC into (ea) */

  pcflags &= ~(PC_FPD|PC_TRAP1|PC_TRAP2|PC_AFI);

  setpc(increa(ea));		/* Jump to ea+1 */
}

INST(jsp, 0265)
{
  if (pcsection)
    {
      dpb (5, 6, 0, AC);
      dpb (35, 30, (pcsection | pc), AC);
    }
  else
    {
      dpb (17, 18, pcflags << 5, AC);
      dpb (35, 18, pc, AC);
    }

  pcflags &= ~(PC_FPD|PC_TRAP1|PC_TRAP2|PC_AFI);

  setpc (ea);
}

INST(jsa, 0266)
{
  vstore (ea, AC);
  dpb (17, 18, ea, AC);		/* ea,, */
  dpb (35, 18, pc, AC);		/* ea,,pc */

  ea = increa (ea);
  setpc (ea);			/* Jump to ea + 1 */
}

INST(jra, 0267)
{
  addr10 acaddr;

  acaddr = upper18 (AC);	/* Where AC is saved */

  vfetch (acaddr, AC);		/* Restore AC */

  setpc (ea);			/* Jump to EA */
}
