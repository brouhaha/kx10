/* Shift and rotate instructions for kx10, the PDP-10 emulator.
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

INST(ash, 0240)
{
  int shiftamount;

  shiftamount = ea & 0377;
  if (ea & 0400000)
    shiftamount |= ~(unsigned long)0377;

  ashift(shiftamount, AC);
}

/* We implement rotation by doing a shift left by the amount, and then a
 * shift right by 36-amount, and oring the results together.  It makes me
 * gag too...
 */

INST(rot, 0241)
{
  int shiftamount;
  word36 tmp;

  shiftamount = (ea & 0377);
  if (ea & 0400000)
    shiftamount |= ~(unsigned long)0377;

  if (shiftamount >= 36
      || shiftamount <= -36)
  shiftamount %= 36;

  tmp = AC;
  shift(shiftamount, AC);
  if (shiftamount > 0)
    {
      shift(shiftamount-36, tmp);
    }
  else
    {
      shift(shiftamount+36, tmp);
    }
  ior36(AC, AC, tmp);
}

INST(lsh, 0242)
{
  int shiftamount;

  shiftamount = ea & 0377;
  if (ea & 0400000)
    shiftamount |= ~(unsigned long)0377;

  shift(shiftamount, AC);
}

INST(ashc, 0244)
{
  int shiftamount;
  word36 hiac, loac, tmp;

  shiftamount = ea & 0377;
  if (ea & 0400000)
    shiftamount |= ~(unsigned long)0377;

  if (shiftamount > 70)
    shiftamount = 70;
  else if (shiftamount < -70)
    shiftamount = -70;

  if (shiftamount == 0)
    return;

  hiac = AC;
  loac = ACplus1;

  if (shiftamount > 0)
    {				/* Left shift */
      ashift(shiftamount, hiac); /* Shift the upper part */
      tmp = loac;
      dpb(0, 1, 0, tmp);
      shift(shiftamount - 35, tmp); /* Create low bits of upper part */
      ior36(hiac, hiac, tmp);		/* Install low bits of upper part */
      dpb(0, 1, 0, loac);
      shift(shiftamount, loac); /* Shift lower part */
    }
  else if (shiftamount < 0)
    {				/* Right shift */
      dpb(0, 1, 0, loac);
      shift(shiftamount, loac); /* Shift lower part */
      tmp = hiac;
      dpb(0, 1, 0, tmp);
      shift(shiftamount + 35, tmp); /* Create high bits of lower part */
      ior36(loac, loac, tmp);	/* Install high bits of lower part */
      ashift(shiftamount, hiac); /* Shift upper part */
    }

  if (biton(0, hiac))
    {
      dpb(0, 1, 1, loac);
    }
  else
    {
      dpb(0, 1, 0, loac);
    }

  AC = hiac;
  ACplus1 = loac;
}

INST(rotc, 0245)
{
  int shiftamount;
  word36 hiac, loac, tmp;

  shiftamount = ea & 0377;
  if (ea & 0400000)
    shiftamount |= ~(unsigned long)0377;

  if (shiftamount >= 72
      || shiftamount <= -72)
    shiftamount %= 72;

  hiac = AC;
  loac = ACplus1;

  if (shiftamount > 36)		/* big left rotate */
    {
      AC = ACplus1;		/* shift by 36 */
      shift (shiftamount - 36, AC); /* shift by amt - 36 */
      ACplus1 = zero36;		/* zapp the lower part */

      tmp = hiac;
      shift (-(72 - shiftamount), hiac); /* right shift upper */
      shift (36 - (72 - shiftamount), tmp);
      shift (-(72 - shiftamount), loac); /* right shift and lower */
      ior36 (loac, loac, tmp);
    }
  else if (shiftamount >= 0)	/* small left rotate */
    {
      tmp = ACplus1;
      shift (shiftamount, AC);
      shift (shiftamount, ACplus1);
      shift (-(36 - shiftamount), tmp);
      ior36 (AC, AC, tmp);

      shift (-((72 - shiftamount) - 36), hiac); /* shift by amt - 36 */
      loac = hiac;		/* shift by 36 */
      hiac = zero36;		/* zapp the lower part */
    }
  else if (shiftamount >= -36)	/* Small right rotate */
    {
      tmp = AC;			/* Doing right shift */
      shift (shiftamount, AC);
      shift (shiftamount, ACplus1);
      shift (36 + shiftamount, tmp);
      ior36 (ACplus1, ACplus1, tmp);

      shift ((72 + shiftamount) - 36, loac); /* shift by amt - 36 */
      hiac = loac;		/* shift by 36 */
      loac = zero36;		/* zapp the lower part */
    }
  else				/* big right rotate */
    {
      ACplus1 = AC;		/* shift by 36 */
      shift (36 + shiftamount, ACplus1); /* shift by amt - 36 */
      AC = zero36;		/* zapp the lower part */

      tmp = loac;
      shift (72 + shiftamount, hiac); /* left shift upper */
      shift (-(36 - (72 + shiftamount)), tmp);
      shift (72 + shiftamount, loac); /* left shift and lower */
      ior36 (hiac, hiac, tmp);
    }

  ior36 (AC, AC, hiac);
  ior36 (ACplus1, ACplus1, loac);
}

INST(lshc, 0246)
{
  int shiftamount;
  word36 hiac, loac, tmp;

  hiac = AC;
  loac = ACplus1;

  shiftamount = ea & 0377;
  if (ea & 0400000)
    shiftamount |= ~(unsigned long)0377;

  if (shiftamount >= 72
      || shiftamount <= -72)
    shiftamount %= 72;

  if (shiftamount > 0)
    {				/* Left shift */
      shift(shiftamount, hiac);	/* Shift the upper part */
      tmp = loac;
      shift(shiftamount - 36, tmp); /* Create low bits of upper part */
      ior36(hiac, hiac, tmp);	/* Install low bits of upper part */
      shift(shiftamount, loac);	/* Shift lower part */
    }
  else if (shiftamount < 0)
    {				/* Right shift */
      shift(shiftamount, loac);	/* Shift lower part */
      tmp = hiac;
      shift(36 + shiftamount, tmp); /* Create high bits of lower part */
      ior36(loac, loac, tmp);		/* Install high bits of upper part */
      shift(shiftamount, hiac);	/* Shift upper part */
    }
  else
    return;

  AC = hiac;
  ACplus1 = loac;
}
