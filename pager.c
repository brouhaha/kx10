/* Pager emulation routines for kx10, the PDP-10 emulator.
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

int setting_pc = 0;

struct tlbent
{
  word36 *read_pointer;
  word36 *write_pointer;
};

struct tlb
{
  int high_water;
  int user;			/* 1 if user mode, 0 if kernel */
  struct tlbent tlbents[32 * 512];
  unsigned int tlb[32 * 512];
};

static struct tlb kernel_tlb;
static struct tlb user_tlb;

static struct tlb *current_tlb;

static struct tlb *current_tlb_ea;

static struct tlb *current_tlb_1;

static struct tlb *current_tlb_2;

int saved_pcsection = -1;

void
gouser ()
{
  current_tlb = &user_tlb;

  current_tlb_ea = &user_tlb;

  current_tlb_1 = &user_tlb;

  current_tlb_2 = &user_tlb;

  pc_cache_vm = -1;
}

void
goexec ()
{
  current_tlb = &kernel_tlb;

  current_tlb_ea = &kernel_tlb;

  current_tlb_1 = &kernel_tlb;

  current_tlb_2 = &kernel_tlb;

  pc_cache_vm = -1;
}

void
set_previous_context_func (func)
     int func;
{
  if (func & 1)
    {
      mem_acblock_1 = prevacblock;
      if (pcflags & PC_PCU)
	{
	  current_tlb_1 = &user_tlb;
	}
    }
  if (func & 2)
    {
      mem_acblock_2 = prevacblock;
      ea_acblock = prevacblock;
      if (pcflags & PC_PCU)
	{
	  current_tlb_2 = &user_tlb;

	  current_tlb_ea = &user_tlb;
	}
    }
  else
    {
      ea_acblock = currentacblock;
      current_tlb_ea = &kernel_tlb;
    }

  if (func & 4)
    {
      mem_acblock = prevacblock;
      if (pcflags & PC_PCU)
	{
	  current_tlb = &user_tlb;
	}
    }
}

void
clear_previous_context ()
{
  mem_acblock = currentacblock;
  mem_acblock_1 = currentacblock;
  mem_acblock_2 = currentacblock;
  ea_acblock = currentacblock;

  goexec ();
}

void
set_previous_eacalc ()
{
  ea_acblock = prevacblock;
  if (pcflags & PC_PCU)
    {
      current_tlb_ea = &user_tlb;
    }
}

void
clear_tlb (clear_keep)
     int clear_keep;
{
  int i;
  struct tlb *tlb;

  tlb = &kernel_tlb;

  if (clear_keep)
    {
#if 0
      for (i = 0; i < tlb->high_water; i++)
	{
	  tlb->read_pointers[i] = 0;
	  tlb->write_pointers[i] = 0;
	  tlb->tlb[i] = 0;
	}
#else
      i = tlb->high_water * sizeof (struct tlbent);
      bzero (tlb->tlbents, i);
      bzero (tlb->tlb, tlb->high_water * sizeof (tlb->tlb[0]));
#endif
      tlb->high_water = 0;
    }
  else
    {
      for (i = 0; i < tlb->high_water; i++)
	{
	  if (tlb->tlb[i] & PF_KEEP)
	    continue;
	  tlb->tlbents[i].read_pointer = 0;
	  tlb->tlbents[i].write_pointer = 0;
	  tlb->tlb[i] = 0;
	}
    }

  tlb = &user_tlb;

#if 0
  for (i = 0; i < tlb->high_water; i++)
    {
      tlb->read_pointers[i] = 0;
      tlb->write_pointers[i] = 0;
      tlb->tlb[i] = 0;
    }
#else
  i = tlb->high_water * sizeof (struct tlbent);
  bzero (tlb->tlbents, i);
  bzero (tlb->tlb, tlb->high_water * sizeof (tlb->tlb[0]));
#endif
  tlb->high_water = 0;

  pc_cache_vm = -1;
}

void
pager_init ()
{
  user_tlb.high_water = 32 * 512;
  user_tlb.user = 1;

  kernel_tlb.high_water = 32 * 512;
  kernel_tlb.user = 0;

  clear_tlb (1);
  goexec ();
}

/* Read a word from the EPT */

void
efetch(offset, loc)
     int offset;
     word36 *loc;
{
  pfetch (((pagerconi & PAGEBR) << 9) + offset, *loc);
}

void
estore(offset, loc)
     int offset;
     word36 loc;
{
  pstore (((pagerconi & PAGEBR) << 9) + offset, loc);
}

/* Read a word from the UPT */

void
ufetch (offset, loc)
     int offset;
     word36 *loc;
{
  pfetch (((pagerdatailow & 017777) << 9) + offset, *loc);
}

void
ustore (offset, loc)
     int offset;
     word36 loc;
{
  pstore (((pagerdatailow & 017777) << 9) + offset, loc);
}

/* Call this routine to turn on flags in pcflags that will cause a trap */

extern trace_ubr;

void
setflags (newflags)
     int newflags;
{
  int opcode, ac;
  addr10 ea;
  word36 ir;
  int tmp;

  pcflags |= newflags;

  if (!(pagerconi & PAGENA))
    return;

  if (!(pcflags & (PC_TRAP1 | PC_TRAP2)))
    return;

  trapflags = pcflags & (PC_TRAP1 | PC_TRAP2);
  pcflags &= ~(PC_TRAP1 + PC_TRAP2);

#ifdef PROCESS_TRACING
  if (pagerdatailow == trace_ubr
      && pcflags & PC_USER)
    printf ("Setting flags: %o\r\n", newflags);
#endif

  tmp = (trapflags >> 2) & 3;

  if (pcflags & PC_USER)
    ufetch (0420 + tmp, &ir);
  else
    efetch (0420 + tmp, &ir);

  opcode = ldb (8, 9, ir);
  ac = ldb (12, 4, ir);
  ea = ieacalc(ir, pcsection);	/* Calc ea in the section of the inst */
  (*opdisp[opcode])(ac, ea); /* Recursively call the interpreter */

  trapflags = 0;
}

#define VM

int tops10_paging = 0;

static void hard_fault PARAMS ((addr10 loc, unsigned int pageinfo, int user,
			int is_pc)) NORETURN;
static void
hard_fault (loc, pageinfo, user, is_pc)
     addr10 loc;
     unsigned int pageinfo;
     int user;
     int is_pc;
{
  word36 tmp;

  if (saved_pcsection != -1)
    {
      pcsection = saved_pcsection;
      saved_pcsection = -1;
    }

  if ((pageinfo & (PF_HARD | PF_ACCESSIBLE)) == PF_ACCESSIBLE
      && (pageinfo & PF_PAGE) >= MEMPAGES)
    {
      pageinfo &= ~PF_HARD_ERROR_MASK;
      pageinfo |= PF_AR_PARITY_ERROR;
      set_nxm(loc, pageinfo);	/* Set non-existant memory indication */
    }

  if (user)
    pageinfo |= PF_USER;

#ifdef PROCESS_TRACING
  if (pagerdatailow == trace_ubr
      && pcflags & PC_USER)
    printf ("Page fault at %o, pagerflags = %o\r\n", loc, pageinfo);
#endif

  if (!(pagerconi & PAGKL))
    {
      unsigned int tmp;

/* Need to relocate writaeable and soft bits for KI style paging */

      tmp = pageinfo;
      pageinfo &= ~(PF_MODIFIED | PF_WRITEABLE | PF_SOFT);
      if (tmp & PF_WRITEABLE)
	pageinfo |= 040000000;
      if (tmp & PF_SOFT)
	pageinfo |= 020000000;
    }

  dpb(8, 9, pageinfo >> 18, tmp);
  dpb(35, 27, loc, tmp);
  ustore(0500, tmp);		/* Page fail word */

  dpb (17, 18, (pcflags | trapflags) << 5, tmp);
  trapflags = 0;

  /* Back up PC so that we restart faulting instructions, but don't do this
     if fault occurred during instruction fetch.  */
  if (!is_pc && !setting_pc)
    pc = (pc - 1) & HWORDMASK;

  if (!(pagerconi & PAGKL))
    {
      dpb (35, 18, pc, tmp);
      ustore (0501, tmp);		/* Fault PC  & flags */
    }
  else
    {
      dpb (35, 18, prev_pcsection >> 18, tmp);
      ustore (0501, tmp);	/* flags */

      dpb (5, 6, 0, tmp);
      dpb (35, 30, pc | pcsection, tmp);
      ustore (0502, tmp);	/* Fault PC */
    }

  goexec ();
  if (pcflags & PC_USER)
    {
      pcflags &= ~PC_USER;	/* Now in kernel mode */
      pcflags |= PC_PCU;	/* Prev context was user */
    }
  else
    pcflags &= ~PC_PCU;		/* Prev context wasn't user */

  if (!(pagerconi & PAGKL))
    {
      ufetch (0502, &tmp);
      setpc (lower18 (tmp));
    }
  else
    {
      ufetch(0503, &tmp);		/* Trap handler */
      setpc(ldb(35, 30, tmp));
    }

  siglongjmp(to_main, 1);
}

void
illegal_indirect_trap (ea, is_pc)
     addr10 ea;
     int is_pc;
{
  hard_fault (ea, PF_HARD | (4 << 21), pcflags & PC_USER, is_pc);
}

static int update_cst PARAMS ((unsigned int pagenum, int writeref));

static int
update_cst (pagenum, writeref)
     register unsigned int pagenum;
     int writeref;
{
  if (ne36 (CSTAC)
      && (pagerconi & (PAGENA | PAGKL)) == (PAGENA | PAGKL))
    { 
      addr10 loc;
      word36 mem;
      int age;

      loc = ldb(35, 22, CSTAC) + pagenum;
      pfetch (loc, mem);
      age = ldb (5, 6, mem);
      if (age == 0)
	return 0;
      and36 (mem, mem, CSTMASKAC);
      ior36 (mem, mem, CSTDATAAC);
#if 1
      if (writeref)
	{
	  if (!biton(18, mem))
	    return 0;
	  dpb (35, 1, 1, mem);
	}
#endif
      pstore (loc, mem);
    }
  return 1;
}

static unsigned int page_fault PARAMS ((unsigned int pagenum, int user));

static unsigned int
page_fault (pagenum, user)
     register unsigned int pagenum;
     int user;
{
  unsigned int faultpage;
  unsigned int faultsection;
  word36 secptr;
  unsigned int ptrtype;
  unsigned int prot = 017;		/* Public, writable, keep, cacheable */
  unsigned int storagemedium;
  unsigned int index;
  unsigned int sptindex;
  unsigned int page;

  if (pagerconi & PAGENA)	/* Is the pager on? */
    {
      if (!(pagerconi & PAGKL))
	{
	  word36 tmp;
	  unsigned int mapent;

	  if (user)
	    ufetch (pagenum >> 1, &tmp);
	  else
	    if (pagenum >= 0400)
	      efetch (pagenum >> 1, &tmp);
	    else if (pagenum >= 0340)
	      ufetch (((pagenum - 0340) >> 1) + 0400, &tmp);
	    else
	      efetch ((pagenum >> 1) + 0600, &tmp);

	  if (pagenum & 1)
	    mapent = lower18 (tmp);
	  else
	    mapent = upper18 (tmp);

	  page = (mapent & PF_PAGE) | PF_KEEP;
	  if (mapent & 0400000)
	    page |= PF_ACCESSIBLE;
	  if (mapent & 0200000)
	    page |= PF_PUBLIC;
	  if (mapent & 0100000)
	    page |= PF_WRITEABLE;
	  if (mapent & 040000)
	    page |= PF_SOFT;
	  if (mapent & 020000)
	    page |= PF_CACHEABLE;

	  return page;
	}

      faultsection = pagenum >> 9;
      faultpage = pagenum & 0777;

      if (user)
	ufetch (0540+faultsection, &secptr);
      else
	efetch (0540+faultsection, &secptr);

      /* Loop on section pointers */
    secnextind:
      ptrtype = ldb(2, 3, secptr);
      prot &= ldb(6, 4, secptr);
      switch (ptrtype)
	{
	case 0:			/* Paged out ?? XXX */
	  return 0;
	case 1:			/* Direct pointer */
	  storagemedium = ldb(17, 6, secptr);
	  if (storagemedium != 0)
	    return 0;
	  page = ldb(35, 13, secptr);
	  break;
	case 2:			/* Shared pointer */
	  sptindex = lower18(secptr);
	  pfetch (ldb(35, 22, SPTAC) + sptindex, secptr);
	  storagemedium = ldb(17, 6, secptr);
	  if (storagemedium != 0)
	    return 0;		/* XXX */
	  page = ldb(35, 13, secptr);
	  break;
	case 3:			/* Indirect pointer */
	  sptindex = lower18(secptr);
	  index = ldb(17, 9, secptr);
	  pfetch (ldb(35, 22, SPTAC) + sptindex, secptr);
	  storagemedium = ldb(17, 6, secptr);
	  if (storagemedium != 0)
	    return 0;		/* XXX */
	  page = ldb(35, 13, secptr);
	  if (!update_cst(page, 0))
	    return 0;
	  pfetch ((page << 9) | index, secptr);
	  goto secnextind;
	default:
	  printf("Unhandled section pointer type 0%o\r\n", ptrtype);
	  genunimp();
	  return 0;
	}

      /* Now, lookup page */

      if (!update_cst(page, 0))	/* Page table */
	return 0;
      pfetch((page << 9) + faultpage, secptr);

      /* Loop on page pointers */
    nextind:
      ptrtype = ldb(2, 3, secptr);
      prot &= ldb(6, 4, secptr);
      switch (ptrtype)
	{
	case 0:			/* Paged out ?? XXX */
	  return 0;
	case 1:			/* Direct pointer */
	  page = ldb(35, 13, secptr);
	  storagemedium = ldb(17, 6, secptr);
	  if (storagemedium != 0)
	    return 0;
	direct:
	  page |= PF_ACCESSIBLE;
	  if (prot & 01)
	    page |= PF_CACHEABLE;
	  if (prot & 02)
	    page |= PF_KEEP;
	  if (prot & 04)
	    page |= PF_WRITEABLE;
	  if (prot & 010)
	    page |= PF_PUBLIC;
/*	  page |= PF_VIRTUAL;*/
	  return page;
	case 2:			/* Shared pointer */
	  sptindex = lower18(secptr);
	  pfetch (ldb(35, 22, SPTAC) + sptindex, secptr);
	  storagemedium = ldb(17, 6, secptr);
	  if (storagemedium != 0)
	    return 0;		/* XXX */
	  page = ldb(35, 13, secptr);
	  goto direct;
	case 3:			/* Indirect pointer */
	  sptindex = lower18(secptr);
	  index = ldb(17, 9, secptr);
	  pfetch (ldb(35, 22, SPTAC) + sptindex, secptr);
	  storagemedium = ldb(17, 6, secptr);
	  if (storagemedium != 0)
	    return 0;		/* XXX */
	  page = ldb(35, 13, secptr);
	  if (!update_cst (page, 0))
	    return 0;
	  pfetch ((page << 9) | index, secptr);
	  goto nextind;
	default:
	  printf("Unhandled page pointer type 0%o\r\n", ptrtype);
	  genunimp();
	  return 0;
	}
    }
  else
    return PF_ACCESSIBLE | PF_WRITEABLE | PF_PUBLIC | PF_CACHEABLE | pagenum;
}

word36
map (loc)
     addr10 loc;
{
  unsigned int pageinfo;
  unsigned int pagenum;
  struct tlb *tlb;
  word36 val;

  tlb = current_tlb;
  pagenum = (loc & KLADDRMASK) >> 9;

  if (tlb->tlb[pagenum])
    pageinfo = tlb->tlb[pagenum];
  else
    {
      pageinfo = page_fault (pagenum, tlb->user);

      if (pagenum >= tlb->high_water)
	tlb->high_water = pagenum + 1;

      tlb->tlb[pagenum] = pageinfo;
    }

  if (!(pagerconi & PAGKL))
    {
      unsigned int tmp;

/* Need to relocate writaeable and soft bits for KI style paging */

      tmp = pageinfo;
      pageinfo &= ~(PF_MODIFIED | PF_WRITEABLE | PF_SOFT);
      if (tmp & PF_WRITEABLE)
	pageinfo |= 040000000;
      if (tmp & PF_SOFT)
	pageinfo |= 020000000;
    }

  dpb(26, 27, pageinfo, val);	/* access & page number */

  dpb(35, 9, loc, val);		/* word within page */

  return val;
}

/* Clear the page table entry associated with LOC */

void
clrpt(loc)
     addr10 loc;
{
  unsigned int pagenum;

  pagenum = (loc & KLADDRMASK) >> 9;
  current_tlb->tlbents[pagenum].read_pointer = 0;
  current_tlb->tlbents[pagenum].write_pointer = 0;
  current_tlb->tlb[pagenum] = 0;
  if (((pcsection | pc) & KLPAGEMASK) == pc_cache_vm)
    pc_cache_vm = -1;
}

static word36 *
read_refill(loc, tlb, is_pc)
     addr10 loc;
     struct tlb *tlb;
     int is_pc;
{
  unsigned int pageinfo;
  unsigned int pagenum;
  word36 *physaddr;

  pagenum = loc >> 9;

  if (tlb->tlb[pagenum])
    pageinfo = tlb->tlb[pagenum];
  else
    {
      pageinfo = page_fault (pagenum, tlb->user);

      if (pagenum >= tlb->high_water)
	tlb->high_water = pagenum + 1;

      tlb->tlb[pagenum] = pageinfo;
    }

  if ((pageinfo & (PF_HARD | PF_ACCESSIBLE)) == PF_ACCESSIBLE
      && (pageinfo & PF_PAGE) < MEMPAGES)
    {
      if (!update_cst (pageinfo & PF_PAGE, 0))
	hard_fault (loc, pageinfo, tlb->user, is_pc);

      physaddr = memarray + ((pageinfo & PF_PAGE) << 9)	- (loc & 037777000);

      tlb->tlbents[pagenum].read_pointer = physaddr;

      /* Come here when reference should succeed */

      return tlb->tlbents[pagenum].read_pointer;
    }
  else
    hard_fault (loc, pageinfo, tlb->user, is_pc);
}

static word36 *
write_refill(loc, tlb)
     addr10 loc;
     struct tlb *tlb;
{
  unsigned int pageinfo;
  unsigned int pagenum;
  word36 *physaddr;

  pagenum = loc >> 9;

  if (tlb->tlb[pagenum])
    pageinfo = tlb->tlb[pagenum];
  else
    {
      pageinfo = page_fault (pagenum, tlb->user);

      if (pagenum >= tlb->high_water)
	tlb->high_water = pagenum + 1;

      tlb->tlb[pagenum] = pageinfo;
    }

  if (((pageinfo & (PF_HARD | PF_ACCESSIBLE | PF_WRITEABLE))
       == (PF_ACCESSIBLE | PF_WRITEABLE))
      && (pageinfo & PF_PAGE) < MEMPAGES)
    {
      if (!update_cst(pageinfo & PF_PAGE, 1))
	hard_fault (loc, pageinfo | PF_WRITEREF, tlb->user, 0);

      tlb->tlb[pagenum] |= PF_MODIFIED;

      physaddr = memarray + ((pageinfo & PF_PAGE) << 9)	- (loc & 037777000);

      tlb->tlbents[pagenum].write_pointer = physaddr;

      tlb->tlbents[pagenum].read_pointer = physaddr;

      /* Come here when reference should succeed */

      return tlb->tlbents[pagenum].write_pointer;
    }
  else
    hard_fault(loc, pageinfo | PF_WRITEREF, tlb->user, 0);
}

void
Vfetch_i(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

#if 0
  if (loc > KLADDRMASK)
    hard_fault (loc, PF_HARD | (7 << 21), current_tlb->user, 1);
#endif

  if (loc & 0777760)		/* section local address? */
    {				/* No, do it the hard way */
      pagenum = (loc >> 9) & (KLPAGEMASK >> 9);
      tlb_ent = current_tlb->tlbents[pagenum].read_pointer;

      if (tlb_ent != 0)
	*word = *(tlb_ent + loc);
      else
	{
	  tlb_ent = read_refill(loc, current_tlb, 1);
	  *word = *(tlb_ent + loc);
	}
    }
  else
    *word = mem_acref(loc);
}

word36 *
vfetch_i_ref (loc)
     register addr10 loc;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  pagenum = (loc >> 9) & (KLPAGEMASK >> 9);
  tlb_ent = current_tlb->tlbents[pagenum].read_pointer;

  if (tlb_ent != 0)
    return tlb_ent;
  else
    return read_refill(loc, current_tlb, 1);
}

void
Vfetch(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    *word = mem_acref(loc);
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb->tlbents[pagenum].read_pointer;

      if (tlb_ent == 0)
	tlb_ent = read_refill(loc, current_tlb, 0);
      *word = *(tlb_ent + loc);
    }
}

void
Vstore(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    mem_acref(loc) = *word;
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb->tlbents[pagenum].write_pointer;

      if (tlb_ent == 0)
	tlb_ent = write_refill(loc, current_tlb);
      *(tlb_ent + loc) = *word;
    }
}

/* Return a pointer to the physical memory associated with LOC.  This function
   makes sure that the location is read/write.  If not, it faults.  This could
   be used to speed up code that makes several references to the same page by
   avoiding some calls into the VM system.  */

word36 *
vref_rw (loc)
     register addr10 loc;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
   return &mem_acref (loc);
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb->tlbents[pagenum].write_pointer;

      if (tlb_ent == 0)
	tlb_ent = write_refill(loc, current_tlb);
      return tlb_ent + loc;
    }
}

void
Vfetch_ea(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    *word = mem_acref(loc);
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb_ea->tlbents[pagenum].read_pointer;

      if (tlb_ent == 0)
	tlb_ent = read_refill(loc, current_tlb_ea, 0);
      *word = *(tlb_ent + loc);
    }
}

void
Vfetch_1(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    *word = mem_acref_1(loc);
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb_1->tlbents[pagenum].read_pointer;

      if (tlb_ent == 0)
	tlb_ent = read_refill(loc, current_tlb_1, 0);
      *word = *(tlb_ent + loc);
    }
}

void
Vstore_1(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    mem_acref_1(loc) = *word;
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb_1->tlbents[pagenum].write_pointer;

      if (tlb_ent == 0)
	tlb_ent = write_refill(loc, current_tlb_1);
      *(tlb_ent + loc) = *word;
    }
}

void
Vfetch_2(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    *word = mem_acref_2(loc);
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb_2->tlbents[pagenum].read_pointer;

      if (tlb_ent == 0)
	tlb_ent = read_refill(loc, current_tlb_2, 0);
      *word = *(tlb_ent + loc);
    }
}

void
Vstore_2(loc, word)
     register addr10 loc;
     register word36 *word;
{
  register unsigned int pagenum;
  word36 *tlb_ent;

  if (is_acref(loc))
    mem_acref_2(loc) = *word;
  else
    {
      loc &= KLADDRMASK;
      pagenum = loc >> 9;
      tlb_ent = current_tlb_2->tlbents[pagenum].write_pointer;

      if (tlb_ent == 0)
	tlb_ent = write_refill(loc, current_tlb_2);
      *(tlb_ent + loc) = *word;
    }
}
