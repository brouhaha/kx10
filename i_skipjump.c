/* Skip and jump instructions for kx10, the PDP-10 emulator.
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
    
/* This file contains all of the Tx, SKIPx, CAx, JUMPx SOSx, AOSx, SOJx, AOJx,
 * SOBJx, and AOBJx instructions.
 */

#include "pdp10.h"

INST(aobjp, 0252)
{
  register int count, ptr;

  count = upper18(AC) + 1;
  ptr = lower18(AC) + 1;

  dpb(17, 18, count, AC);
  dpb(35, 18, ptr, AC);

  if (!(count & 0400000)) setpc(ea);
}

INST(aobjn, 0253)
{
  register int count, ptr;

  count = upper18(AC) + 1;
  ptr = lower18(AC) + 1;

  dpb(17, 18, count, AC);
  dpb(35, 18, ptr, AC);

  if (count & 0400000) setpc(ea); /* Jump if count is negative */
}
