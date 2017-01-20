/* Stack instructions for kx10, the PDP-10 emulator.
   Copyright (C) 1991, 1992, 1993, 1994, 1995  Stu Grossman

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

INST(adjsp, 0105)
{
  int count;
  addr10 addr;

  count = upper18(AC);

  if (pcsection && count <= 0377777 && count != 0)
    {
      if (ea & 0400000)
	ea |= ~HWORDMASK; /* Sign extend ea */
      else
	ea &= HWORDMASK;

      addr = ldb(35, 30, AC);	/* Get 30 bit stack addr from ac */

      addr += ea;		/* Decr stack pointer */
      dpb(35, 30, addr, AC);	/* Update AC */
    }
  else
    {
      addr = lower18 (AC);	/* Get the stack addr */

      count += ea;		/* Adjust the count */
      addr += ea;

      dpb(17, 18, count, AC);	/* Update the AC */
      dpb(35, 18, addr, AC);

      /* XXX Deal with stack overflow/underflow */
    }
}

INST(pushj, 0260)
{
  word36 tmp;
  int count;
  addr10 addr;

  count = upper18(AC);

  if (pcsection && count <= 0377777 && count != 0)
    {
      dpb (35, 30, (pcsection | pc), tmp); /* 30 bit pc for non-zero sections */
      dpb (5, 6, 0, tmp);

      addr = (ldb(35, 30, AC) + 1) | GLOBAL;
      vstore(addr, tmp);
      dpb(35, 30, addr, AC);
    }
  else
    {
      if (!pcsection)
	{
	  dpb (17, 18, pcflags << 5, tmp); /* Generate PC and flags for sect 0 */
	  dpb (35, 18, pc, tmp);
	}
      else
	{
	  dpb (35, 30, (pcsection | pc), tmp); /* 30 bit pc for non-zero sections */
	  dpb (5, 6, 0, tmp);
	}

      count++;			/* Inc counter and addr, defaulting section */
      addr = pcsection | ((lower18 (AC) + 1) & HWORDMASK);

      vstore(addr, tmp);	/* Store PC & flags into addr */

      dpb(17, 18, count, AC);	/* Update the AC */
      dpb(35, 18, addr, AC);

      if ((count & HWORDMASK) == 0)
	{			/* Check for overflow trap */
	  printf("Got a PDL OV!!!\n");
	}
    }

  pcflags &= ~(PC_FPD|PC_TRAP1|PC_TRAP2|PC_AFI);

  setpc(ea);			/* Jump to ea */
}

INST(push, 0261)
{
  word36 mem;
  int count;
  addr10 addr;

  vfetch(ea, mem);		/* Get object of push */

  count = upper18(AC);

  if (pcsection && count <= 0377777 && count != 0)
    {
      addr = (ldb(35, 30, AC) + 1) | GLOBAL;
      vstore_1(addr, mem);
      dpb(35, 30, addr, AC);
    }
  else
    {
      count++;			/* Inc counter and addr, defaulting section */
      addr = pcsection | ((lower18 (AC) + 1) & HWORDMASK);

      vstore_1(addr, mem);	/* Store (ea) into addr */

      dpb(17, 18, count, AC);	/* Update the AC */
      dpb(35, 18, addr, AC);

      if ((count & HWORDMASK) == 0)
	{			/* Check for overflow trap */
	  printf("Got a PDL OV!!!\n");
	}
    }
}

INST(pop, 0262)
{
  word36 mem;
  int count;
  addr10 addr;

  count = upper18(AC);

  if (pcsection && count <= 0377777 && count != 0)
    {
      addr = ldb(35, 30, AC);	/* Get 30 bit stack addr from ac */
      vfetch_1(addr | GLOBAL, mem); /* Get word from stack */
      vstore(ea, mem);		/* Store it into (ea)*/

      addr--;			/* Decr stack pointer */
      dpb(35, 30, addr, AC);	/* Update AC */
    }
  else
    {
      addr = lower18(AC);	/* Get the addr to get the word from */

      vfetch_1(pcsection | addr, mem); /* Fetch word from the stack */
      vstore(ea, mem);		/* Store it into (ea)*/

      count--;			/* Decrement count and addr */
      addr = pcsection | ((addr - 1) & HWORDMASK);

      dpb(17, 18, count, AC);	/* Update the AC */
      dpb(35, 18, addr, AC);

      if ((count & HWORDMASK) == HWORDMASK)
	{			/* Did we underflow?  Trap */
	  printf("Got a PDL underflow!!!\n");
	}
    }
}

INST(popj, 0263)
{
  word36 tmp;
  int count;
  addr10 addr;

  count = upper18(AC);

  if (pcsection && count <= 0377777 && count != 0)
    {
      addr = ldb(35, 30, AC);	/* Get 30 bit stack addr from ac */
      vfetch(addr | GLOBAL, tmp); /* Get pc from stack addr */

      addr--;			/* Decr stack pointer */
      dpb(35, 30, addr, AC);	/* Update AC */

      setpc(ldb(35, 30, tmp));	/* Load up 30 bit PC */
    }
  else
    {
      addr = lower18 (AC);	/* Get the addr to get the PC from */

      vfetch(pcsection | addr, tmp); /* Fetch PC and flags from addr */

      count--;			/* Decrement count and addr */
      addr = pcsection | ((addr - 1) & HWORDMASK);

      dpb(17, 18, count, AC);	/* Update the AC */
      dpb(35, 18, addr, AC);

      if ((count & HWORDMASK) == HWORDMASK)
	{			/* Did we underflow? */
				/* trap */
	  printf("Got a PDL underflow!!!\n");
	}

      if (pcsection == 0)
	setpc(lower18 (tmp));	/* PC from lower half */
      else
	setpc(ldb(35, 30, tmp)); /* Load up 30 bit PC */
    }
}
