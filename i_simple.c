/* Simple instructions for kx10, the PDP-10 emulator.
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

/* This file contains all of the really simple instructions, like doubleword,
 * fullword, and halfword instructions.  It also contains the booleans.
 */

#include "pdp10.h"

INST(dadd, 0114)
{
  word36 mem0, mem1;

  vfetch (ea, mem0);
  ea = increa (ea);
  vfetch (ea, mem1);

  add72_flags (AC, ACplus1, mem0, mem1);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(dsub, 0115)
{
  word36 mem0, mem1;

  vfetch (ea, mem0);
  ea = increa (ea);
  vfetch (ea, mem1);

  not36 (mem0, mem0);
  not36 (mem1, mem1);

  incr72_flags (mem0, mem1);

  add72_flags (AC, ACplus1, mem0, mem1);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(dmove, 0120)
{
  word36 tmp, tmp1;

  vfetch(ea, tmp);
  ea = increa(ea);
  vfetch(ea, tmp1);

  AC = tmp;
  ACplus1 = tmp1;
}

INST(dmovn, 0121)
{
  word36 mem0, mem1;

  vfetch (ea, mem0);
  ea = increa (ea);
  vfetch (ea, mem1);

  not36 (mem0, mem0);
  not36 (mem1, mem1);

  incr72_flags (mem0, mem1);
  dpb (0, 1, 0, mem1);		/* Sign bit in 2nd word is always clear */

  AC = mem0;
  ACplus1 = mem1;

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(dmovem, 0124)
{
  word36 tmp, tmp1;

  tmp = AC;
  tmp1 = ACplus1;

  vstore (ea, tmp);
  ea = increa (ea);
  vstore (ea, tmp1);
}

INST(dmovnm, 0125)
{
  word36 mem0, mem1;

  not36 (mem0, AC);
  not36 (mem1, ACplus1);

  incr72_flags (mem0, mem1);
  dpb (0, 1, 0, mem1);		/* Sign bit in 2nd word is always clear */

  vstore (ea, mem0);
  ea = increa (ea);
  vstore (ea, mem1);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(move, 0200)
{
  vfetch(ea, AC);
}

INST(movei, 0201)
{
  dpb(17, 18, 0, AC);
  dpb(35, 18, ea, AC);
}

INST(movem, 0202)
{
  vstore(ea, AC);
}

INST(moves, 0203)
{
  word36 mem;

  vfetch(ea, mem);
  vstore(ea, mem);

  if (ac)
    AC = mem;
}

INST(movs, 0204)
{
  word36 mem;

  vfetch(ea, mem);

  dpb(17, 18, lower18(mem), AC);
  dpb(35, 18, upper18(mem), AC);
}

INST(movsi, 0205)
{
  dpb(17, 18, ea, AC);
  dpb(35, 18, 0, AC);
}

INST(movsm, 0206)
{
  word36 tmp;

  dpb(17, 18, lower18(AC), tmp);
  dpb(35, 18, upper18(AC), tmp);

  vstore(ea, tmp);
}

INST(movss, 0207)
{
  word36 mem, tmp;

  vfetch(ea, mem);

  dpb(17, 18, lower18(mem), tmp);
  dpb(35, 18, upper18(mem), tmp);

  vstore(ea, tmp);

  if (ac)
    AC = tmp;
}

INST(movn, 0210)
{
  word36 mem;

  vfetch(ea, mem);

  not36 (AC, mem);
  incr36_flags(AC);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(movni, 0211)
{
  word36 tmp;

  tmp = zero36;

  dpb(35, 18, ea, tmp);

  not36 (AC, tmp);
  incr36_flags(AC);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(movnm, 0212)
{
  word36 mem;

  not36 (mem, AC);
  incr36_flags(mem);

  vstore(ea, mem);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(movns, 0213)
{
  word36 mem;

  vfetch(ea, mem);

  not36 (mem, mem);
  incr36_flags(mem);

  vstore(ea, mem);

  if (ac) AC = mem;

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(movm, 0214)
{
  word36 mem;

  vfetch(ea, mem);

  if (lt36(mem))
    {
      not36 (mem, mem);
      incr36_flags(mem);
    }

  AC = mem;

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(movmi, 0215)
{
  word36 tmp;

  dpb(17, 18, 0, AC);
  dpb(35, 18, ea, AC);

  /* Can never be < 0, so no need to negate or set flags. */
}

INST(movmm, 0216)
{
  word36 mem;

  mem = AC;

  if (lt36(mem))
    {
      not36 (mem, mem);
      incr36_flags(mem);
    }

  vstore(ea, mem);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(movms, 0217)
{
  word36 mem;

  vfetch(ea, mem);

  if (lt36(mem))
    {
      not36 (mem, mem);
      incr36_flags(mem);
    }

  vstore(ea, mem);

  if (ac) AC = mem;

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(exch, 0250)
{
  word36 tmp;

  vfetch(ea, tmp);		/* Get memory contents */
  vstore(ea, AC);	/* Store ac contents in memory */
  AC = tmp;		/* Put memory into ac */
}
