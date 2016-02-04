/* Effective address calculation routines for kx10, the PDP-10 emulator.
   Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995  Stu Grossman

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

/* Calculate the effective address of an EFIW (Extended Format Indirect
 * Word).  About the only thing that calls this is the code for handling
 * two-word global byte pointers.
 */

addr10
efiw_eacalc(efiw, section)
     word36 efiw;
     addr10 section;
{
  int i=0, x, y, global=0;
  addr10 ea;

  ea = section;

  if (ge36 (efiw))
    goto globalind;

 localind:			/* Here to handle local indirect words */

  if (biton (1, efiw))
    illegal_indirect_trap (ea, 1);

  i = biton(13, efiw);
  x = ldb(17, 4, efiw);
  y = lower18(efiw);

  if (x)
    {
      if ((ea & SECTMASK) && !biton(0, ea_acref(x)) && ldb(17, 12, ea_acref(x)))
	{
	  ea = (ldb(35, 30, ea_acref(x)) + ((y & 0400000) ? (y | SECTMASK) : y))
	    & ADDRMASK;
	  global = GLOBAL;
	}
      else
	{
	  ea = (ea & SECTMASK) | ((lower18(ea_acref(x)) + y) & HWORDMASK);
	  global = 0;
	}
    }
  else
    ea = (ea & SECTMASK) | y;

  if (!i)
    return(ea | global);

 nextind:			/* Here for each indirect word */

  /* Check for interrupts at each indirect fetch.   This prevents a hard
     hang when y==. && i is on, ie: `move t1,@.'  */

  if (interrupt >= 0)
    {
      pc = (pc - 1) & HWORDMASK; /* Continue at this instruction */
      siglongjmp(to_main, 1);
    }

  vfetch_ea (ea|global, efiw);

  if ((ea & SECTMASK) == 0)
    goto localind;

  if (biton(0, efiw))
    {
      global = 0;
      goto localind;
    }

  /* Global indirect word */

 globalind:

  global = GLOBAL;
  i = biton(1, efiw);
  x = ldb(5, 4, efiw);
  y = ldb(35, 30, efiw);

  if (x)
    ea = (ldb(35, 30, ea_acref(x)) + y) & ADDRMASK;
  else
    ea = y;

  if (i)
    goto nextind;

  return(ea|global);
}

/* Calculate the effective address of an IFIW (Instruction Format Indirect
 * Word)
 */

addr10
Ieacalc(ir, section)
     word36 ir;
     addr10 section;
{
  int i=0, x, y, global=0;
  addr10 ea;

  ea = section;

 localind:			/* Here to handle local indirect words */

  i = biton(13, ir);
  x = ldb(17, 4, ir);
  y = lower18(ir);

  if (x)
    {
      if ((ea & SECTMASK) && !biton(0, ea_acref(x)) && ldb(17, 12, ea_acref(x)))
	{
	  ea = (ldb(35, 30, ea_acref(x)) + ((y & 0400000) ? (y | SECTMASK) : y))
	    & ADDRMASK;
	  global = GLOBAL;
	}
      else
	{
	  ea = (ea & SECTMASK) | ((lower18(ea_acref(x)) + y) & HWORDMASK);
	  global = 0;
	}
    }
  else
    ea = (ea & SECTMASK) | y;

  if (!i)
    return(ea | global);

 nextind:			/* Here for each indirect word */

  /* Check for interrupts at each indirect fetch.   This prevents a hard
     hang when y==. && i is on, ie: `move t1,@.'  */

  if (interrupt >= 0)
    {
      pc = (pc - 1) & HWORDMASK; /* Continue at this instruction */
      siglongjmp(to_main, 1);
    }

  vfetch_ea (ea|global, ir);

  if ((ea & SECTMASK) == 0)
    goto localind;

  if (biton(0, ir))
    {
      if (biton (1, ir))
	illegal_indirect_trap (ea, 1);

      global = 0;
      goto localind;
    }

  /* Global indirect word */

  global = GLOBAL;
  i = biton(1, ir);
  x = ldb(5, 4, ir);
  y = ldb(35, 30, ir);

  if (x)
    ea = (ldb(35, 30, ea_acref(x)) + y) & ADDRMASK;
  else
    ea = y;

  if (i)
    goto nextind;

  return(ea|global);
}

/* This special form of eacalc is required to make jrstf happy */

addr10
jrstfeacalc(ir, section, flags)
     word36 ir;
     addr10 section;
     int *flags;
{
  int i=0, x, y, global=0;
  addr10 ea;

  ea = section;

 localind:			/* Here to handle local indirect words */

  i = biton(13, ir);
  x = ldb(17, 4, ir);
  y = lower18(ir);

  if (x)
    {
      *flags = ldb (12, 13, acref(x));
      if ((ea & SECTMASK) && !biton(0, ea_acref(x)) && ldb(17, 12, ea_acref(x)))
	{
	  ea = (ldb(35, 30, ea_acref(x)) + ((y & 0400000) ? (y | SECTMASK) : y))
	    & ADDRMASK;
	  global = GLOBAL;
	}
      else
	{
	  ea = (ea & SECTMASK) | ((lower18(ea_acref(x)) + y) & HWORDMASK);
	  global = 0;
	}
    }
  else
    ea = (ea & SECTMASK) | y;

  if (!i)
    return(ea | global);

 nextind:			/* Here for each indirect word */

  /* Check for interrupts at each indirect fetch.   This prevents a hard
     hang when y==. && i is on, ie: `jrstf @.'  */

  if (interrupt >= 0)
    {
      pc = (pc - 1) & HWORDMASK; /* Continue at this instruction */
      siglongjmp(to_main, 1);
    }

  vfetch_ea (ea|global, ir);

  *flags = ldb (12, 13, ir);

  if ((ea & SECTMASK) == 0)
    goto localind;

  if (biton(0, ir))
    {
      if (biton (1, ir))
	illegal_indirect_trap (ea, 1);

      global = 0;
      goto localind;
    }

  /* Global indirect word */

  global = GLOBAL;
  i = biton(1, ir);
  x = ldb(5, 4, ir);
  y = ldb(35, 30, ir);

  if (x)
    {
      *flags = ldb(12, 13, ea_acref(x));
      ea = (ldb(35, 30, ea_acref(x)) + y) & ADDRMASK;
    }
  else
    ea = y;

  if (i)
    goto nextind;

  return(ea|global);
}
