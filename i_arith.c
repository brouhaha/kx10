/* Fixed and floating point instructions for kx10, the PDP-10 emulator.
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

/* This file contains all of the fixed and floating point arithmetic
 * instructions.
 */

#include "pdp10.h"

/* Calculate the 70 bit product of MP and MC.  Put the result in *HI and *LO
   in the standard 70 bit format.  Return 1 if overflow, 0 otherwise. */

static int multiply PARAMS ((word36 mp, word36 mc, word36 *phi, word36 *plo));

static int
multiply (mp, mc, phi, plo)
     register word36 mp, mc;
     register word36 *phi, *plo;
{
  unsigned short u[4], v[4], w[7];
  register unsigned int t, k, sign;
  register int i, j;

  u[1] = ldb(3, 4, mp);
  u[2] = ldb(19, 16, mp);
  u[3] = ldb(35, 16, mp);
  if (u[1] & 010)               /* Negative? */
    {
      sign = 1;

      k = (unsigned short)~u[3] + 1;
      u[3] = k;
      k = (unsigned short)~u[2] + (k >> 16);
      u[2] = k;
      u[1] = (u[1] ^ 017) + (k >> 16);
    }
  else
    sign = 0;

  v[1] = ldb(3, 4, mc);
  v[2] = ldb(19, 16, mc);
  v[3] = ldb(35, 16, mc);
  if (v[1] & 010)               /* Negative? */
    {
      sign ^= 1;

      k = (unsigned short)~v[3] + 1;
      v[3] = k;
      k = (unsigned short)~v[2] + (k >> 16);
      v[2] = k;
      v[1] = (v[1] ^ 017) + (k >> 16);
    }

  for (i = 1; i <= 6; i++)
    w[i] = 0;

  for (j = 3; j > 0; j--)
    {
      k = 0;
      for (i = 3; i > 0; i--)
	{
	  t = u[i] * v[j] + w[i+j] + k;
	  w[i+j] = t;
	  k = t >> 16;
	}
      w[j] = k;
    }

  if (sign)               /* Negative? */
    {
      k = (unsigned short)~w[6] + 1;
      w[6] = k;
      k = (unsigned short)~w[5] + (k >> 16);
      w[5] = k;
      k = (unsigned short)~w[4] + (k >> 16);
      w[4] = k;
      k = (unsigned short)~w[3] + (k >> 16);
      w[3] = k;
      w[2] = ~w[2] + (k >> 16);
    }

  dpb(6, 7, w[2], *phi);
  dpb(22, 16, w[3], *phi);
  dpb(35, 13, w[4] >> 3, *phi);
  dpb(3, 4, (w[4] & 07) | ((w[2] >> 3) & 010), *plo);
  dpb(19, 16, w[5], *plo);
  dpb(35, 16, w[6], *plo);

/* The only overflow case (-2^35 * -2^35) results in an excessivly large
   positive number (2^70), which shows up as w[2] == 0100.  */

  if (w[2] == 0100)		/* Positive and too large? */
    return 0;			/* We overflowed */
  else
    return 1;			/* No overflow */
}

/* Calculate the 140 bit product of MP and MC.  Put the result in *HI and *LO
   in the standard 70 bit format.  Return 1 if overflow, 0 otherwise. */

static inline int unpack_72 PARAMS ((unsigned short *dest_array,
				     word36 hi_word, word36 lo_word));
static inline int
unpack_72 (dest_array, hi_word, lo_word)
     unsigned short *dest_array;
     word36 hi_word, lo_word;
{
  int k;

  dest_array[1] = ldb (6, 7, hi_word);
  dest_array[2] = ldb (22, 16, hi_word);
  dest_array[3] = ldb (35, 13, hi_word) << 3;
  dest_array[3] |= ldb (3, 3, lo_word);
  dest_array[4] = ldb (19, 16, lo_word);
  dest_array[5] = ldb (35, 16, lo_word);

  if (!(dest_array[1] & 0100))
    return 0;			/* Positive, we're all done */

  /* Negative.  Make positive. */

  k = (unsigned short)~dest_array[5] + 1;
  dest_array[5] = k;
  k = (unsigned short)~dest_array[4] + (k >> 16);
  dest_array[4] = k;
  k = (unsigned short)~dest_array[3] + (k >> 16);
  dest_array[3] = k;
  k = (unsigned short)~dest_array[2] + (k >> 16);
  dest_array[2] = k;
  dest_array[1] = (dest_array[1] ^ 0177) + (k >> 16);

  return 1;
}

static int multiply_72 PARAMS ((word36 mp_hi, word36 mp_lo, word36 mc_hi, word36 mc_lo,
				word36 *p3, word36 *p2, word36 *p1, word36 *p0));
static int
multiply_72 (mp_hi, mp_lo, mc_hi, mc_lo, p3, p2, p1, p0)
     register word36 mp_hi, mp_lo;
     register word36 mc_hi, mc_lo;
     register word36 *p3, *p2, *p1, *p0;
{
  unsigned short u[6], v[6], w[11];
  register unsigned int t, k, sign;
  register int i, j;

  sign = unpack_72 (u, mp_hi, mp_lo);

  sign ^= unpack_72 (v, mc_hi, mc_lo);

  memset (w, '\000', sizeof w);

  for (j = 5; j > 0; j--)
    {
      k = 0;
      for (i = 5; i > 0; i--)
	{
	  t = u[i] * v[j] + w[i+j] + k;
	  w[i+j] = t;
	  k = t >> 16;
	}
      w[j] = k;
    }

  if (sign)               /* Negative? */
    {
      k = (unsigned short)~w[10] + 1;
      w[10] = k;
      k = (unsigned short)~w[9] + (k >> 16);
      w[9] = k;
      k = (unsigned short)~w[8] + (k >> 16);
      w[8] = k;
      k = (unsigned short)~w[7] + (k >> 16);
      w[7] = k;
      k = (unsigned short)~w[6] + (k >> 16);
      w[6] = k;
      k = (unsigned short)~w[5] + (k >> 16);
      w[5] = k;
      k = (unsigned short)~w[4] + (k >> 16);
      w[4] = k;
      k = (unsigned short)~w[3] + (k >> 16);
      w[3] = k;
      w[2] = ~w[2] + (k >> 16);
    }

  /* Sign must come from result in case product was 0 or overflowed. */
  sign = w[2] >> 12;

  dpb (12, 13, w[2], *p3);
  dpb (28, 16, w[3], *p3);
  dpb (35, 7, w[4] >> 9, *p3);

  dpb (9, 10, (sign << 9) | (w[4] & 0x1ff), *p2);
  dpb (25, 16, w[5], *p2);
  dpb (35, 10, w[6] >> 6, *p2);

  dpb (6, 7, (sign << 6) | (w[6] & 0x3f), *p1);
  dpb (22, 16, w[7], *p1);
  dpb (35, 13, w[8] >> 3, *p1);

  dpb (3, 4, (sign << 3) | (w[8] & 0x7), *p0);
  dpb (19, 16, w[9], *p0);
  dpb (35, 16, w[10], *p0);

/* The only overflow case (-2^70 * -2^70) results in an excessivly large
   positive number (2^140), which shows up as w[2] == 010000.  */

  if (w[2] == 010000)		/* Positive and too large? */
    return 0;			/* We overflowed */
  else
    return 1;			/* No overflow */
}

/*
  Divide the 70 bit number in uwhi, uwlo by vw storing the quotient in *qw and
  remainder in *rw.  Handles positive and negative numbers.

  Returns 0 if divide by 0 or quotient overflow, 1 otherwise.
*/

static int divide PARAMS ((word36 uwhi, word36 uwlo, word36 vw,
			   word36 *qw, word36 *rw));
static int
divide (uwhi, uwlo, vw, qw, rw)
     word36 uwhi, uwlo, vw;
     word36 *qw, *rw;
{
  unsigned long u[6], v[4], rem;
  unsigned long q[6], r[4];
  unsigned long tmp, t2;
  int d, j, k, leadingzeros;
  const unsigned int base = 0x10000;
  const unsigned int bshift = 16;
  const unsigned int bmask = base - 1;
  int n, m, quotient_sign, remainder_sign;

  if (lt36 (uwhi))		/* Negative? */
    {
      quotient_sign = remainder_sign = 1;

      neg72 (uwhi, uwlo, uwhi, uwlo);
    }
  else
    quotient_sign = remainder_sign = 0;

  u[5] = ldb (35, 16, uwlo);
  u[4] = ldb (19, 16, uwlo);
  u[3] = (ldb (35, 13, uwhi) << 3) | ldb (3, 3, uwlo);
  u[2] = ldb (22, 16, uwhi);
  u[1] = ldb (6, 7, uwhi);
  u[0] = 0;

  if (lt36 (vw))		/* Negative? */
    {
      quotient_sign ^= 1;

      neg36 (vw, vw);
    }

/* If the magnitude of the upper part of the dividend is >= the magnitude of
   the divisor, we would get a quotient overflow.  */

  if (!cltu (uwhi, vw))
    return 0;

  v[3] = ldb (35, 16, vw);
  v[2] = ldb (19, 16, vw);
  v[1] = ldb (3, 4, vw);
  v[0] = 0;

  /* Find first non-zero digit of v */

  n = sizeof v / sizeof v[0] - 1; /* Highest index in v */

  /* Find the highest non-zero element in v */

  for (j = 1; j <= n; j++)
    if (v[j] != 0)
      break;

  if (j > n)
    return 0;			/* Divide by 0! */

  leadingzeros = j - 1;

  /* Shift v left to get rid of leading zeros */

  for (j = 1; j <= n - leadingzeros; j++)
    v[j] = v[leadingzeros + j];

  n -= leadingzeros;

  m = (sizeof u / sizeof u[0] - 1) - n;

  d = base / (v[1] + 1);

  /* Normalize u */

  tmp = 0;
  for (j = n + m; j >= 0; j--)
    {
      t2 = u[j] * d + tmp;
      tmp = t2 >> bshift;
      u[j] = t2 & bmask;
    }

  /* Normalize v */

  tmp = 0;
  for (j = n; j >= 1; j--)
    {
      t2 = v[j] * d + tmp;
      tmp = t2 >> bshift;
      v[j] = t2 & bmask;
    }

  for (j = 0; j <= m ;j++)
    {
      unsigned long qhat, rhat;

      /* Calculate qhat */

      if (u[j] == v[1])
	{
	  qhat = base - 1;
	  rhat = u[j + 1] + v[1];
	}
      else
	{
	  qhat = ((u[j] << bshift) + u[j + 1]) / v[1];
	  rhat = ((u[j] << bshift) + u[j + 1]) % v[1];
	}

      /* If qhat was too big, decrement it */

      while (rhat < base
	     && v[2] * qhat > (rhat << bshift) + u[j + 2])
	{
	  qhat--;
	  rhat += v[1];
	}

      /* Multiply and subtract */

      t2 = 0;
      for (k = n; k >= 0; k--)
	{
	  unsigned long t3;

	  t3 = qhat * v[k] + t2;
	  t2 = t3 >> bshift;
	  t3 &= bmask;
	  u[j + k] -= t3;
	  
	  if (u[j + k] >= base)
	    {			/* Need to borrow */
	      if (k == 0)
		break;		/* Subtract overflowed */
	      u[j + k - 1]--;
	      u[j + k] &= bmask;
	    }
	}

      q[j + n] = qhat;

      if (u[j] >= base)
	{
	  /* qhat was too large...  Decr and correct u */
	  q[j + n]--;
	  for (k = n; k >= 1; k--)
	    {
	      u[j + k] += v[k];
	      if (u[j + k] >= base)
		{		/* Carry */
		  u[j + k - 1]++;
		  u[j + k] &= bmask;
		}
	    }
	}
    }

  /* Calculate the remainder */

  rem = 0;
  for (j = 0; j < n; j++)
    {
      unsigned long x;

      x = (rem << bshift) + u[j + m + 1];
      r[j] = x / d;
      rem = x % d;
    }

  if (quotient_sign)
    {
      q[5] = (q[5] ^ 0177777) + 1;
      q[4] = (q[4] ^ 0177777) + (q[5] >> 16);
      q[3] = (q[3] ^ 0177777) + (q[4] >> 16);
    }

  dpb(35, 16, q[5], *qw);
  dpb(19, 16, q[4], *qw);
  dpb(3, 4, q[3], *qw);

  *rw = zero36;
  switch (n)
    {
    case 1:
      dpb(35, 16, r[0], *rw);
      break;
    case 2:
      dpb(19, 16, r[0], *rw);
      dpb(35, 16, r[1], *rw);
      break;
    case 3:
      dpb(3, 4, r[0] & 7, *rw);
      dpb(19, 16, r[1], *rw);
      dpb(35, 16, r[2], *rw);
      break;
    default:
      abort ();
    }

  if (remainder_sign)
    neg36 (*rw, *rw);

  return 1;
}

/*
  Divide the 140 bit number in uwhi, uwlo by vw storing the quotient in *qw and
  remainder in *rw.  Handles positive and negative numbers.

  Returns 0 if divide by 0 or quotient overflow, 1 otherwise.
*/

static int divide_72 PARAMS ((word36 u0, word36 u1, word36 u2, word36 u3,
			      word36 v_hi, word36 v_lo,
			      word36 *q_hi, word36 *q_lo,
			      word36 *r_hi, word36 *r_lo));
static int
divide_72 (u0, u1, u2, u3, v_hi, v_lo, q_hi, q_lo, r_hi, r_lo)
     word36 u0, u1, u2, u3;
     word36 v_hi, v_lo;
     word36 *q_hi, *q_lo, *r_hi, *r_lo;
{
  unsigned long u[10], v[6], rem;
  unsigned long q[10], r[6];
  unsigned long tmp, t2;
  int d, j, k, leadingzeros;
  const unsigned int base = 0x10000;
  const unsigned int bshift = 16;
  const unsigned int bmask = base - 1;
  int n, m, quotient_sign, remainder_sign;

  if (lt36 (u0))		/* Negative? */
    {
      quotient_sign = remainder_sign = 1;

      neg144 (u0, u1, u2, u3, u0, u1, u2, u3);
    }
  else
    quotient_sign = remainder_sign = 0;

  u[9] = ldb (35, 16, u3);
  u[8] = ldb (19, 16, u3);
  u[7] = ldb (3, 3, u3) | (ldb (35, 13, u2) << 3);
  u[6] = ldb (22, 16, u2);
  u[5] = ldb (6, 6, u2) | (ldb (35, 10, u1) << 6);
  u[4] = ldb (25, 16, u1);
  u[3] = ldb (9, 9, u1) | (ldb (35, 7, u0) << 9);
  u[2] = ldb (28, 16, u0);
  u[1] = ldb (12, 13, u0);
  u[0] = 0;

  if (lt36 (v_hi))		/* Negative? */
    {
      quotient_sign ^= 1;

      neg72 (v_hi, v_lo, v_hi, v_lo);
    }

/* If the magnitude of the upper part of the dividend is >= the magnitude of
   the divisor, we would get a quotient overflow.  */

  dpb (0, 1, 0, v_lo);
  dpb (0, 1, 0, u1);

  if (!(cltu (u0, v_hi)
	|| (ceq (u0, v_hi) && cltu (u1, v_lo))))
    return 0;

  v[5] = ldb (35, 16, v_lo);
  v[4] = ldb (19, 16, v_lo);
  v[3] = (ldb (35, 13, v_hi) << 3) | ldb (3, 3, v_lo);
  v[2] = ldb (22, 16, v_hi);
  v[1] = ldb (6, 6, v_hi);
  v[0] = 0;

  /* Find first non-zero digit of v */

  n = sizeof v / sizeof v[0] - 1; /* Highest index in v */

  /* Find the highest non-zero element in v */

  for (j = 1; j <= n; j++)
    if (v[j] != 0)
      break;

  if (j > n)
    return 0;			/* Divide by 0! */

  leadingzeros = j - 1;

  /* Shift v left to get rid of leading zeros */

  for (j = 1; j <= n - leadingzeros; j++)
    v[j] = v[leadingzeros + j];

  n -= leadingzeros;

  m = (sizeof u / sizeof u[0] - 1) - n;

  d = base / (v[1] + 1);

  /* Normalize u */

  tmp = 0;
  for (j = n + m; j >= 0; j--)
    {
      t2 = u[j] * d + tmp;
      tmp = t2 >> bshift;
      u[j] = t2 & bmask;
    }

  /* Normalize v */

  tmp = 0;
  for (j = n; j >= 1; j--)
    {
      t2 = v[j] * d + tmp;
      tmp = t2 >> bshift;
      v[j] = t2 & bmask;
    }

  for (j = 0; j <= m ;j++)
    {
      unsigned long qhat, rhat;

      /* Calculate qhat */

      if (u[j] == v[1])
	{
	  qhat = base - 1;
	  rhat = u[j + 1] + v[1];
	}
      else
	{
	  qhat = ((u[j] << bshift) + u[j + 1]) / v[1];
	  rhat = ((u[j] << bshift) + u[j + 1]) % v[1];
	}

      /* If qhat was too big, decrement it */

      while (rhat < base
	     && v[2] * qhat > (rhat << bshift) + u[j + 2])
	{
	  qhat--;
	  rhat += v[1];
	}

      /* Multiply and subtract */

      t2 = 0;
      for (k = n; k >= 0; k--)
	{
	  unsigned long t3;

	  t3 = qhat * v[k] + t2;
	  t2 = t3 >> bshift;
	  t3 &= bmask;
	  u[j + k] -= t3;
	  
	  if (u[j + k] >= base)
	    {			/* Need to borrow */
	      if (k == 0)
		break;		/* Subtract overflowed */
	      u[j + k - 1]--;
	      u[j + k] &= bmask;
	    }
	}

      q[j + n] = qhat;

      if (u[j] >= base)
	{
	  /* qhat was too large...  Decr and correct u */
	  q[j + n]--;
	  for (k = n; k >= 1; k--)
	    {
	      u[j + k] += v[k];
	      if (u[j + k] >= base)
		{		/* Carry */
		  u[j + k - 1]++;
		  u[j + k] &= bmask;
		}
	    }
	}
    }

  /* Calculate the remainder */

  rem = 0;
  for (j = 0; j < n; j++)
    {
      unsigned long x;

      x = (rem << bshift) + u[j + m + 1];
      r[j] = x / d;
      rem = x % d;
    }

  dpb (35, 16, q[9], *q_lo);
  dpb (19, 16, q[8], *q_lo);
  dpb (3, 4, q[7] & 07, *q_lo);
  dpb (35, 13, q[7] >> 3, *q_hi);
  dpb (22, 16, q[6], *q_hi);
  dpb (6, 7, q[5] & 0177, *q_hi);

  if (quotient_sign)
    {
      neg72 (*q_hi, *q_lo, *q_hi, *q_lo);
      dpb (0, 1, ldb (0, 1, *q_hi), *q_lo); /* Copy sign bit to lower word */
    }

  *r_hi = zero36;
  *r_lo = zero36;
  switch (n)
    {
    case 1:
      dpb (35, 16, r[0], *r_lo);
      break;
    case 2:
      dpb (19, 16, r[0], *r_lo);
      dpb (35, 16, r[1], *r_lo);
      break;
    case 3:
      dpb (35, 13, r[0] >> 3, *r_hi);
      dpb (3, 4, r[0] & 7, *r_lo);
      dpb (19, 16, r[1], *r_lo);
      dpb (35, 16, r[2], *r_lo);
      break;
    case 4:
      dpb (22, 16, r[0], *r_hi);
      dpb (35, 13, r[1] >> 3, *r_hi);
      dpb (3, 4, r[1] & 7, *r_lo);
      dpb (19, 16, r[2], *r_lo);
      dpb (35, 16, r[3], *r_lo);
      break;
    case 5:
      dpb (6, 6, r[0], *r_hi);
      dpb (22, 16, r[1], *r_hi);
      dpb (35, 13, r[2] >> 3, *r_hi);
      dpb (3, 4, r[2] & 7, *r_lo);
      dpb (19, 16, r[3], *r_lo);
      dpb (35, 16, r[4], *r_lo);
      break;
    case 6:
      dpb (6, 6, r[1], *r_hi);
      dpb (22, 16, r[2], *r_hi);
      dpb (35, 13, r[3] >> 3, *r_hi);
      dpb (3, 4, r[3] & 7, *r_lo);
      dpb (19, 16, r[4], *r_lo);
      dpb (35, 16, r[5], *r_lo);
      break;
    default:
      abort ();
    }

  if (remainder_sign)
    {
      neg72 (*r_hi, *r_lo, *r_hi, *r_lo);
      dpb (0, 1, ldb (0, 1, *r_hi), *r_lo); /* Copy sign bit to lower word */
    }

  return 1;
}

#define FRACTMASK ((unsigned int)((1 << 27) - 1)) /* Mask of entire fraction */
#define FRACTBIT ((unsigned int)(1 << 26)) /* High order bit of fraction */

static inline void float_addsub PARAMS ((const int opcode, const int ac,
					 addr10 ea));

static inline void
float_addsub (opcode, ac, ea)
     const int opcode;
     const int ac;
     addr10 ea;
{
  int expa, expb, sign;
  long fracta, fractb;
  word36 tmp;

/* Extract the exponents and fractions */

  if (lt36 (AC))
    neg36 (tmp, AC)
  else
    tmp = AC;

  expa = ldb (8, 8, tmp);
  fracta = ldb (35, 27, tmp) << 3;
  if (lt36 (AC))
    fracta = -fracta;

/* Get the second operand.  Either fetch or unpack if immediate.  */

  if ((opcode & 07) == 05)
    {				/* Immediate mode operand */
      long xxx;

      xxx = ea;
      if (ea & 0400000)
	xxx = -ea;

      expb = (xxx >> 9) & 0377;
      fractb = (xxx & 0777) << (18 + 3);
      if (ea & 0400000)
	fractb = -fractb;
    }
  else
    {				/* Memory operand */
      word36 mem;

      vfetch (ea, mem);
      tmp = mem;
      if (lt36 (mem))
	neg36 (tmp, mem)

      expb = ldb (8, 8, tmp);
      fractb = ldb (35, 27, tmp) << 3;
      if (lt36 (mem))
	fractb = -fractb;
    }

/* Unnormalize the smaller operand so that exponents are the same */

  if (expa > expb)
    if (expa - expb > 31)
      fractb = 0;
    else
      fractb >>= expa - expb;
  else if (expb > expa)
    {
      if (expb - expa > 31)
	fracta = 0;
      else
	fracta >>= expb - expa;
      expa = expb;
    }

/* Actually perform the operation!  */

  if ((opcode & 0770) == 0150)
    fracta -= fractb;		/* Finally, do the subtraction */
  else
    fracta += fractb;		/* Or addition.  */

  if (fracta < 0)
    {
      sign = 1;
      fracta = -fracta;
    }
  else
    sign = 0;

/* normalize */

  if (fracta & 0x40000000)
    {				/* Fraction too large */
      expa++;
      fracta >>= 1;
    }
  else if (fracta > 0)
    while (!(fracta & 0x20000000))
      {				/* Fraction too small */
	expa--;
	fracta <<= 1;
      }
  else
    {				/* Fraction was zero */
      expa = 0;
      fracta = 0;
    }

/* Round if necessary */

  if (opcode & 004)		/* Rounding requested */
    {
      fracta += 04;		/* Round up */
      if (fracta & 0x40000000)	/* Rounding can unnormalize things */
	{			/* Fraction too large */
	  expa++;
	  fracta >>= 1;
	}
    }
  fracta >>= 3;			/* Get rid of the extra precision */

/* Repack the result */

  dpb (8, 9, expa & 0377, tmp);
  dpb (35, 27, fracta, tmp);
  if (sign)
    neg36 (tmp, tmp);

  switch (opcode & 03)
    {
    case 03:			/* Both? */
      vstore (ea, tmp);		/* Store to memory and fall thru */
    case 00:			/* Regular */
    case 01:			/* Immediate */
      AC = tmp;			/* Yes, both store to AC */
      break;
    case 02:			/* Memory */
      vstore (ea, tmp);
      break;
    }

  if (expa > 255)
    setflags (PC_OV | PC_FOV | PC_TRAP1);
  else if (expa < 0)
    setflags (PC_OV | PC_FOV | PC_FUF | PC_TRAP1);
}

static inline void float_multiply PARAMS ((const int opcode, const int ac,
					   addr10 ea));

static inline void
float_multiply (opcode, ac, ea)
     const int opcode;
     const int ac;
     addr10 ea;
{
  word36 mem, multiplier, multiplicand, prodhi, prodlo, tmp;
  int sign, expa, expb, exptmp;
  unsigned long fracta, fractb, fractmp;

  if (opcode != 0165)		/* fmpri? */
    vfetch (ea, mem);		/* No, fetch second operand */
  else
    {				/* Yes, turn ea into float */
      dpb (17, 18, ea, mem);
      dpb (35, 18, 0, mem);
    }

  sign = 0;

  tmp = AC;
  if (lt36 (AC))
    {
      neg36 (tmp, AC);
      sign = 1;
    }

  if (lt36 (mem))
    {
      neg36 (mem, mem);
      sign ^= 1;
    }

/* Extract the exponents and fractions */

  expa = ldb (8, 8, tmp);
  fracta = ldb (35, 27, tmp);
  expb = ldb (8, 8, mem);
  fractb = ldb (35, 27, mem);

/* Normalize the multiplier and multiplicand, else the product may need to be
   normalized by too large an amount, requiring double-word shifts... */

  if (fracta > 0)
    while (fracta < FRACTBIT)
      {				/* Fraction too small */
	expa--;
	fracta <<= 1;
      }

  if (fractb > 0)
    while (fractb < FRACTBIT)
      {				/* Fraction too small */
	expb--;
	fractb <<= 1;
      }

  exptmp = expa + expb - 128;	/* Calc the new exponent */

  multiplier = multiplicand = zero36;
  dpb (35, 27, fracta, multiplier);
  dpb (35, 27, fractb, multiplicand);

  multiply (multiplier, multiplicand, &prodhi, &prodlo);

  fractmp = (ldb (35, 19, prodhi) << 11) | ldb (11, 11, prodlo);

/* normalize */

  if (fractmp > 0)
    while (!(fractmp & 0x20000000))
      {				/* fraction too small */
	exptmp--;
	fractmp <<= 1;
      }
  else
    {				/* fraction was zero */
      exptmp = 0;
      fractmp = 0;
    }

/* Round if necessary */

  if (opcode & 004)		/* Rounding requested */
    {
      fractmp += 04;		/* Round up */
      if (fractmp & 0x40000000)	/* Rounding can unnormalize things */
	{			/* Fraction too large */
	  expa++;
	  fractmp >>= 1;
	}
    }
  fractmp >>= 3;		/* Get rid of the extra precision */

  dpb (8, 9, exptmp & 0377, tmp);
  dpb (35, 27, fractmp, tmp);

  if (sign)
    neg36 (tmp, tmp)

  switch (opcode & 03)
    {
    case 03:			/* Both? */
      vstore (ea, tmp);		/* Store to memory and fall thru */
    case 00:			/* Regular */
    case 01:			/* Immediate */
      AC = tmp;			/* Yes, both store to AC */
      break;
    case 02:			/* Memory */
      vstore (ea, tmp);
      break;
    }

  if (exptmp > 255)
    setflags (PC_TRAP1 | PC_OV | PC_FOV);
  else if (exptmp < 0)
    setflags (PC_TRAP1 | PC_OV | PC_FOV | PC_FUF);
}

static inline void float_divide PARAMS ((const int opcode, const int ac,
					 addr10 ea));

static inline void
float_divide (opcode, ac, ea)
     const int opcode;
     const int ac;
     addr10 ea;
{
  word36 mem, uhi, ulo, v, q, r, tmp;
  int sign, expa, expb, exptmp, retval;
  unsigned long fracta, fractb, fractmp, remtmp;

  if (opcode != 0175)		/* fdvri? */
    vfetch (ea, mem);		/* No, fetch second operand */
  else
    {				/* Yes, turn ea into float */
      dpb (17, 18, ea, mem);
      dpb (35, 18, 0, mem);
    }

  sign = 0;

  tmp = AC;
  if (lt36 (AC))
    {
      neg36 (tmp, AC);
      sign = 1;
    }

  if (lt36 (mem))
    {
      neg36 (mem, mem);
      sign ^= 1;
    }

/* Extract the exponents and fractions */

  expa = ldb (8, 8, tmp);
  fracta = ldb (35, 27, tmp);
  expb = ldb (8, 8, mem);
  fractb = ldb (35, 27, mem);

  exptmp = expa - expb + 128;	/* Calc the new exponent */

  uhi = ulo = v = zero36;
  dpb (35, 27, fracta, uhi);
  if (fracta >= fractb)
    {
      fractb <<= 1;
      exptmp++;
    }
  dpb (35, 28, fractb, v);
  retval = divide (uhi, zero36, v, &q, &r);

  if (retval == 0)
    {
      setflags (PC_TRAP1 | PC_OV | PC_FOV | PC_NODIV);
      return;
    }

  fractmp = ldb (30, 30, q);	/* Get quotient + 3 rounding bits.  */

/* normalize */

  if (fractmp > 0)
    while (!(fractmp & 0x20000000))
      {				/* fraction too small */
	exptmp--;
	fractmp <<= 1;
      }
  else
    {				/* fraction was zero */
      exptmp = 0;
      fractmp = 0;
    }

  if (opcode & 004)		/* Rounding requested? */
    {
      fractmp += 04;		/* Round up */
      if (fractmp & 0x40000000)	/* Rounding can unnormalize things */
	{			/* Fraction too large */
	  exptmp++;
	  fractmp >>= 1;
	}
    }
  fractmp >>= 3;		/* Get rid of the extra precision */

  dpb (8, 9, exptmp & 0377, tmp);
  dpb (35, 27, fractmp, tmp);

  if (sign)
    {
      if (eq36(r))
        {
          neg36 (tmp, tmp);
        }
      else
        {
          upper18(tmp) = (~upper18(tmp)) & HWORDMASK;
          lower18(tmp) = (~lower18(tmp)) & HWORDMASK;
        }
    }


  switch (opcode & 03)
    {
    case 03:			/* Both? */
      vstore (ea, tmp);		/* Store to memory and fall thru */
    case 00:			/* Regular */
    case 01:			/* Immediate */
      AC = tmp;			/* Yes, both store to AC */
      break;
    case 02:			/* Memory */
      vstore (ea, tmp);
      break;
    }

  if (exptmp > 255)
    setflags (PC_TRAP1 | PC_OV | PC_FOV);
  else if (exptmp < 0)
    setflags (PC_TRAP1 | PC_OV | PC_FOV | PC_FUF);
}

static inline void dp_float_addsub PARAMS ((const int opcode, const int ac,
					    addr10 ea));

static inline void
dp_float_addsub (opcode, ac, ea)
     const int opcode;
     const int ac;
     addr10 ea;
{
  int expa, expb, sign;
  unsigned long fracta_high, fractb_high;
  unsigned long fracta_mid, fractb_mid;
  unsigned long fracta_low, fractb_low;
  unsigned long tmp;
  word36 tmp_high, tmp_low, tmp2_high, tmp2_low;
  addr10 ea2;

  /* Extract the exponents and fractions */

  if (lt36 (AC))
    neg72 (tmp_high, tmp_low, AC, ACplus1)
  else
    {
      tmp_high = AC;
      tmp_low = ACplus1;
    }

  expa = ldb (8, 8, tmp_high);
  fracta_high = (ldb (35, 27, tmp_high) << 3) | ldb (3, 3, tmp_low);
  fracta_mid = ldb (34, 31, tmp_low);
  fracta_low = ldb (35, 1, tmp_low) << 30;
  if (lt36 (AC))
    {
      if ((expa != 0) && ((fracta_high | fracta_mid | fracta_low) == 0))
	fracta_high=0x20000000; 
      fracta_low = ((~fracta_low) & 0x7fffffff) + 1;
      fracta_mid = ((~fracta_mid) & 0x7fffffff) + (fracta_low >> 31);
      fracta_low &= 0x7fffffff; /* Clear overflow bit */
      fracta_high = (~fracta_high + (fracta_mid >> 31));
      fracta_mid &= 0x7fffffff;	/* Clear overflow bit */
    }

  /* Get the second operand. */

  vfetch (ea, tmp_high);
  vfetch (increa (ea), tmp_low);

  if (lt36 (tmp_high))
    {
      neg72 (tmp2_high, tmp2_low, tmp_high, tmp_low);
    }
  else
    {
      tmp2_high = tmp_high;
      tmp2_low = tmp_low;
    }

  expb = ldb (8, 8, tmp2_high);
  fractb_high = (ldb (35, 27, tmp2_high) << 3) | ldb (3, 3, tmp2_low);
  fractb_mid = ldb (34, 31, tmp2_low);
  fractb_low = ldb (35, 1, tmp2_low) << 30;
  /* If subtraction, negate 2nd operand */
  if (((opcode != 0111) && lt36 (tmp_high)) || ((opcode == 0111) && !lt36 (tmp_high)))
    {
      fractb_low = ((~fractb_low) & 0x7fffffff) + 1;
      fractb_mid = ((~fractb_mid) & 0x7fffffff) + (fractb_low >> 31);
      fractb_low &= 0x7fffffff; /* Clear overflow bit */
      fractb_high = (~fractb_high + (fractb_mid >> 31));
      fractb_mid &= 0x7fffffff; /* Clear overflow bit */
    }

  /* Unnormalize the smaller operand so that exponents are the same */

  if (expa > expb)
    if (expa - expb > 62)
      {
	fractb_high = 0;
	fractb_mid = 0;
	fractb_low = 0;
      }
    else
      if (expa - expb > 31)
	{
	  fractb_low=((fractb_mid >> (expa - expb - 31)) | (fractb_high << (expa - expb - 62))) & 0x7ffffff;
	  fractb_mid=((fractb_high >> (expa - expb - 31)) | 
		      ((1 << (expa - expb - 32)) - 1) << (32 + 31 - (expa - expb))) &
			0x7fffffff;

	  if (fractb_high & 0x40000000)
	    fractb_high=0xffffffff;
	  else
	    fractb_high=0;
	}
      else
      {
	fractb_low >>= expa - expb;
	fractb_low |= fractb_mid << (31 - (expa - expb));
	fractb_low &= 0x7fffffff;
	fractb_mid >>= expa - expb;
	fractb_mid |= (fractb_high << (31 - (expa - expb)));
	fractb_mid &= 0x7fffffff;
	if (fractb_high & 0x40000000)
	  fractb_high = (fractb_high >> (expa - expb)) |
	      ((1 << (expa - expb)) - 1) << (32 - (expa - expb)); /* sign ext */
	else
	  fractb_high >>= expa - expb;
      }
  else if (expb > expa)
    {
      if (expb - expa > 62)
	{
	  fracta_high = 0;
	  fracta_mid = 0;
	  fracta_low = 0;
	}
      else
      if (expb - expa > 31)
	{
	  fracta_low=((fracta_mid >> (expb - expa - 31)) | (fracta_high << (expb - expa - 62))) & 0x7ffffff;
	  fracta_mid=((fracta_high >> (expb - expa - 31)) | 
		      ((1 << (expb - expa - 32)) - 1) << (32 + 31 - (expb - expa))) &
			0x7fffffff;

	  if (fracta_high & 0x40000000)
	    fracta_high=0xffffffff;
	  else
	    fracta_high=0;
	}
      else
	{
	  fracta_low >>= expb - expa;
	  fracta_low |= fracta_mid << (31 - (expb - expa));
	  fracta_low &= 0x7fffffff;
	  fracta_mid >>= expb - expa;
	  fracta_mid |= (fracta_high << (31 - (expb - expa)));
	  fracta_mid &= 0x7fffffff;
	  if (fracta_high & 0x40000000)
	    fracta_high = (fracta_high >> (expb - expa)) |
	      ((1 << (expb - expa)) - 1) << (32 - (expb - expa)); /* sign ext */
	  else
	    fracta_high >>= expb - expa;
	}
      expa = expb;
    }

  /* Actually perform the operation!  */

  fracta_low += fractb_low;
  fracta_mid += (fracta_low >> 31) + fractb_mid;
  fracta_low &= 0x7fffffff;
  fracta_high += (fracta_mid >> 31) + fractb_high;
  fracta_mid &= 0x7fffffff;

  if (fracta_high&0x80000000)
    {
      sign = 1;
      fracta_low = ((~fracta_low) & 0x7fffffff) + 1;
      fracta_mid = ((~fracta_mid) & 0x7fffffff) + (fracta_low >> 31);
      fracta_low &= 0x7fffffff;
      fracta_high = ~fracta_high + (fracta_mid >> 31);
      fracta_mid &= 0x7fffffff;
    }
  else
    sign = 0;

  /* normalize */

  if (fracta_high & 0x40000000)
    {				/* Fraction too large */
      expa++;
      fracta_low >>=1;
      fracta_low |= (fracta_mid & 0x1) << 30;
      fracta_mid >>=1;
      fracta_mid |= (fracta_high & 0x1) << 30;
      fracta_high >>= 1;
    }
  else if ((fracta_high | fracta_mid | fracta_low) > 0)
    while (!(fracta_high & 0x20000000))
      {				/* Fraction too small */
	expa--;
	fracta_high <<= 1;
	fracta_high |= fracta_mid >> 30;
	fracta_mid = ((fracta_mid << 1) & 0x7fffffff) | (fracta_low >> 30);
	fracta_low <<=1;
	fracta_low &= 0x7fffffff;
      }
  else
    {				/* Fraction was zero */
      expa = 0;
      fracta_high = 0;
      fracta_mid = 0;
      fracta_low = 0;
    }

  /* Always round in DP */
  fracta_low += 0x20000000;
  fracta_mid += (fracta_low >> 31);
  fracta_low &= 0x7fffffff;
  fracta_high += (fracta_mid >> 31);
  fracta_mid &= 0x7fffffff;

  /* Maybe renormalize */
  if (fracta_high & 0x40000000)
    {				/* Fraction too large */
      expa++;
      fracta_low >>=1;
      fracta_low |= (fracta_mid & 0x1) << 30;
      fracta_mid >>=1;
      fracta_mid |= (fracta_high & 0x1) << 30;
      fracta_high >>= 1;
    }

  /* Repack the result */

  dpb (8, 9, expa & 0377, tmp_high);
  dpb (35, 27, (fracta_high & 0x3ffffff8) >> 3, tmp_high);
  dpb (3, 3, (fracta_high & 0x7), tmp_low);
  dpb (34, 31, fracta_mid, tmp_low);
  dpb (35, 1, (fracta_low >> 30) & 1, tmp_low);
  if (sign)
    neg72 (tmp_high, tmp_low, tmp_high, tmp_low);
  dpb (0,1,0,tmp_low);

  AC = tmp_high;
  ACplus1 = tmp_low;

  if (expa > 255)
    setflags (PC_OV | PC_FOV | PC_TRAP1);
  else if (expa < 0)
    setflags (PC_OV | PC_FOV | PC_FUF | PC_TRAP1);
}

static inline void dp_float_multiply PARAMS ((const int opcode, const int ac,
					    addr10 ea));

static inline void
dp_float_multiply (opcode, ac, ea)
     const int opcode;
     const int ac;
     addr10 ea;
{
  int expa, expb, expr, sign;
  word36 terma_high, terma_low, termb_high, termb_low;
  word36 tmp_high, tmp_low, tmp2_high, tmp2_low;
  word36 r_0,r_1,r_2,r_3;

  unsigned long rw1, rw2, rw3;

  addr10 ea2;

  /* Extract the exponents and fractions */

  sign=0;
  if (lt36 (AC))
    {
      neg72 (tmp_high, tmp_low, AC, ACplus1);
      sign=1;
    }
  else
    {
      tmp_high = AC;
      tmp_low = ACplus1;
    }

  expa = ldb (8, 8, tmp_high);
  upper18(terma_high) = ldb (24, 16, tmp_high);
  lower18(terma_high) = (ldb (35, 11, tmp_high) << (18 - 11)) | (ldb (7, 7, tmp_low));
  upper18(terma_low)  = ldb (24, 17, tmp_low);
  lower18(terma_low)  = ldb (35, 11, tmp_low) << (18 - 11);
  
  /* Get the second operand. */

  vfetch (ea, tmp_high);
  vfetch (increa(ea), tmp_low);

  if (lt36 (tmp_high))
    {
      neg72 (tmp2_high, tmp2_low, tmp_high, tmp_low);
      sign ^= 1;
    }
  else
    {
      tmp2_high=tmp_high;tmp2_low=tmp_low;
    }

  expb = ldb (8, 8, tmp2_high);
  upper18(termb_high) = ldb (24, 16, tmp2_high);
  lower18(termb_high) = (ldb (35, 11, tmp2_high) << (18 - 11)) | (ldb (7, 7, tmp2_low));
  upper18(termb_low)  = ldb (24, 17, tmp2_low);
  lower18(termb_low)  = ldb (35, 11, tmp2_low) << (18 - 11);

  /* Counting on pre-normalized terms.. */

  multiply_72 (terma_high, terma_low, termb_high, termb_low, &r_0, &r_1, &r_2, &r_3);

  /* Calculate the new exponent. */
  expr = expa + expb - 128 - 1;

  /* Extract our result fraction, rounding it at the same time. */
  rw1 = ldb (34, 32, r_0);
  rw2 = ((ldb (35, 1, r_0) << (31 - 1)) | ldb (30, 30, r_1)); /* Skip extra sign */
  rw3 = ((ldb (35, 5, r_1) << (31 - 5)) | ldb (26, 26, r_2));

  if (rw1 & 0x80000000)
    {
      expr++;
      rw3 |= (rw2 << 31);
      rw3 >>= 1;
      rw2 |= (rw1 << 31);
      rw2 >>= 1;
      rw1 >>= 1;
    }
  if ((rw1 | rw2 | rw3) > 0)
    while (!(rw1 & 0x40000000))
      {
	expr--;
	rw1 <<= 1;
	rw2 <<= 1;
	rw3 <<= 1;
	rw1 |= (rw2 >> 31);
	rw2 |= (rw3 >> 31);
	rw2 &= 0x7fffffff;
	rw3 &= 0x7fffffff;
	}
    else
      {
	expr = 0;
	rw1 = 0;
	rw2 = 0;
      }
  
  rw3 += 0x40000000;
  rw2 += rw3 >> 31;
  rw3 &= 0x7fffffff;
  rw1 += rw2 >> 31;
  rw2 &= 0x7fffffff;

  /* May have to re-normalize the result, because of the rounding */
  if (rw1 & 0x80000000)
    {
      expr++;
      rw3 |= (rw2 << 31);
      rw3 >>= 1;
      rw2 |= (rw1 << 31);
      rw2 >>= 1;
      rw1 >>= 1;
    }

  tmp_high=tmp_low=zero36;
  dpb (8, 9, expr & 0377, tmp_high);
  dpb (35, 27, (rw1 & 0x7ffffff0) >> 4, tmp_high);
  dpb (4, 4, (rw1 & 0xf), tmp_low);
  dpb (35, 31, rw2, tmp_low);
  if (sign)
    neg72 (tmp_high, tmp_low, tmp_high, tmp_low);  

  AC = tmp_high;
  ACplus1 = tmp_low;

  if (expr > 255)
    setflags (PC_TRAP1 | PC_OV | PC_FOV);
  else if (expr < 0)
    setflags (PC_TRAP1 | PC_OV | PC_FOV | PC_FUF);
}

static inline void dp_float_divide PARAMS ((const int opcode, const int ac,
					    addr10 ea));

static inline void
dp_float_divide (opcode, ac, ea)
     const int opcode;
     const int ac;
     addr10 ea;
{
  int expa, expb, expr, sign, retval;
  word36 terma_1, terma_2, terma_3, terma_4, termb_high, termb_low;
  word36 tmp_high, tmp_low, tmp2_high, tmp2_low;
  word36 q_0,q_1,r_0,r_1;
  
  unsigned long rw1, rw2, rw3;
  
  addr10 ea2;
  
  /* Extract the exponents and fractions */
  
  sign=0;
  if (lt36 (AC))
    {
      neg72 (tmp_high, tmp_low, AC, ACplus1);
      sign=1;
    }
  else
    {
      tmp_high = AC;
      tmp_low = ACplus1;
    }
  
  expa = ldb (8, 8, tmp_high);
  upper18(terma_1) = ldb (24, 16, tmp_high);
  lower18(terma_1) = (ldb (35, 11, tmp_high) << (18 - 11)) | (ldb (7, 7, tmp_low));
  upper18(terma_2)  = ldb (24, 17, tmp_low);
  lower18(terma_2)  = ldb (35, 11, tmp_low) << (18 - 11);
  terma_3 = zero36;
  terma_4 = zero36;
  
  /* Get the second operand. */
  
  vfetch (ea, tmp_high);
  vfetch (increa(ea), tmp_low);
  
  if (lt36 (tmp_high))
    {
      neg72 (tmp2_high, tmp2_low, tmp_high, tmp_low);
      sign ^= 1;
    }
  else
    {
      tmp2_high=tmp_high;tmp2_low=tmp_low;
    }
  
  expb = ldb (8, 8, tmp2_high);
  upper18(termb_high) = ldb (25, 17, tmp2_high);
  lower18(termb_high) = (ldb (35, 10, tmp2_high) << (18 - 10)) | (ldb (8, 8, tmp2_low));
  upper18(termb_low)  = ldb (25, 17, tmp2_low);
  lower18(termb_low)  = ldb (35, 10, tmp2_low) << (18 - 10);
  
  /* Counting on pre-normalized terms.. */
  
  retval = 
    divide_72 (terma_1, terma_2, terma_3, terma_4, termb_high, termb_low, &q_0, &q_1, &r_0, &r_1);
  
  if (retval == 0)
    {
      setflags (PC_TRAP1 | PC_OV | PC_FOV | PC_NODIV);
      return;
    }
  
  /* Calculate the new exponent. */
  expr = expa - expb + 128 + 1;
  
  /* Extract our result fraction*/
  rw1 = ldb (31, 31, q_0);
  rw2 = ((ldb (35, 4, q_0) << (31 - 4)) | ldb (27, 27, q_1)); /* Skip extra sign */
  rw3 = ldb (35, 8, q_1) << (31 - 8) ;

  /* Normalize */
  if ((rw1 | rw2 | rw3) > 0)
    while (!(rw1 & 0x40000000))
      {
	expr--;
	rw1 <<= 1;
	rw2 <<= 1;
	rw3 <<= 1;
	rw1 |= (rw2 >> 31);
	rw2 |= (rw3 >> 31);
	rw2 &= 0x7fffffff;
	rw3 &= 0x7fffffff;
      }
  else
    {
      expr = 0;
      rw1 = 0;
      rw2 = 0;
      rw3 = 0;
    }

  rw3 += 0x40000000;
  rw2 += rw3 >> 31;
  rw3 &= 0x7fffffff;
  rw1 += rw2 >> 31;
  rw2 &= 0x7fffffff;

  /* May have to re-normalize the result, because of the rounding */
  if (rw1 & 0x80000000)
    {
      expr++;
      rw3 |= (rw2 << 31);
      rw3 >>= 1;
      rw2 |= (rw1 << 31);
      rw2 >>= 1;
      rw1 >>= 1;
    }

  tmp_high=tmp_low=zero36;
  dpb (8, 9, expr & 0377, tmp_high);
  dpb (35, 27, (rw1 & 0x7ffffff0) >> 4, tmp_high);
  dpb (4, 4, (rw1 & 0xf), tmp_low);
  dpb (35, 31, rw2, tmp_low);
  if (sign)
    neg72 (tmp_high, tmp_low, tmp_high, tmp_low);  

  AC = tmp_high;
  ACplus1 = tmp_low;

  if (expr > 255)
    setflags (PC_TRAP1 | PC_OV | PC_FOV);
  else if (expr < 0)
    setflags (PC_TRAP1 | PC_OV | PC_FOV | PC_FUF);
}

INST(dfad, 0110)
{
  dp_float_addsub (0110, ac, ea);
}

INST(dfsb, 0111)
{
  dp_float_addsub (0111, ac, ea);
}

INST(dfmp, 0112)
{
  dp_float_multiply (112, ac, ea);
}

INST(dfdv, 0113)
{
  dp_float_divide (113, ac, ea);
}

static inline void fix PARAMS ((int opcode, int ac, addr10 ea));

static inline void
fix (opcode, ac, ea)
     int opcode;
     int ac;
     addr10 ea;
{
  word36 mem, tmp;
  int exp_shift, sign;

  vfetch (ea, mem);

  sign = 0;
  if (lt36 (mem))
    {
      neg36 (mem, mem);
      sign = 1;
    }

  exp_shift = ldb (8, 8, mem) - 128 - 27;
  if (exp_shift > 35 - 27)
    {
      setflags (PC_OV | PC_TRAP1);
      return;
    }

  dpb (8, 9, 0, mem);
  tmp = mem;

  shift (exp_shift, mem);

  if (opcode & 004		/* Rounding requested? */
      && exp_shift < 0		/* Shifting out bits? */
      && biton (36 + exp_shift, tmp)) /* >= 1/2 lsb? */
    {
      if (sign == 0)
	{			/* For positive, all we care about is */
	  incr36 (mem);		/* lsb >= 1/2. */
	}
      else
	{			/* For negative, lsb must be > 1/2. */
	  int xxx;

	  xxx = ldb (35, -exp_shift - 1, tmp); /* Get lower bits - lsb-1 */
	  if (xxx)		/* If fraction > 1/2, we need to round */
	    {
	      incr36 (mem);
	    }
	}
    }

  if (sign)
    neg36 (mem, mem);

  AC = mem;
}

INST(fix, 0122)
{
  fix (0122, ac, ea);
}

INST(fixr, 0126)
{
  fix (0126, ac, ea);
}

INST(fltr, 0127)		/* Float & round */
{
  word36 mem;
  int exp, i, sign;
  unsigned long fract;

  vfetch(ea, mem);

  if (eq36 (mem))
    {
      AC = zero36;
      return;
    }

  sign = 0;
  if (lt36(mem))
    {
      sign = 1;
      neg36 (mem, mem);
    }

  for (i = 0; i <= 35; i++)
    if (biton (i, mem))
      break;

  shift (i - 8, mem);		/* Normalize fraction + 1 rounding bit */
  fract = ldb (35, 28, mem);	/* Extract fraction */
  exp = 36 - i + 128;		/* Compute exponent */
  fract++;			/* Round up */
  fract >>= 1;			/* Get rid of extra precision */

/* normalize */

  if (fract > FRACTMASK)
    while (fract > FRACTMASK) { /* fraction too large */
      exp++;
      fract >>= 1;
    }
  else if (fract > 0)
    while (fract < FRACTBIT) { /* fraction too small */
      exp--;
      fract <<= 1;
    }
  else {			/* fraction was zero */
    exp = 0;
    fract = 0;
  };

  dpb (8, 9, exp & 0377, AC);
  dpb (35, 27, fract, AC);
  if (sign)
    neg36 (AC, AC);
}

INST(fsc, 0132)
{
  int expa, scale;
  unsigned long fracta;
  int sign;
  
  sign = 0;
  if (lt36 (AC))
    {
      sign = 1;
      neg36 (AC, AC);
    }

  fracta = ldb (35, 27, AC);	/* Get fraction */
  expa = ldb (8, 8, AC);	/* Get exponent */

  if (fracta == 0)
    {
#if 0
      /* this is what the spec says */
      zero (AC);
      return;
#else
      /* This seems to be what the original did */
      if (sign)			/* if the number is negative */
	{
	  fracta = FRACTBIT;	/* then set the fraction to 1 */
	  if (expa == 0)	/* special case 400000,,0 */
	    dpb (0, 1, 0, AC);	/* clear sign bit */
	}
      else
	{
	  zero (AC);
	  return;
	}
#endif
    }

  scale = ea & 0377;
  if (ea & 0400000)
    scale -= 0400;		/* Sign extend */

  expa += scale;		/* Scale exponent! */

/* Normalize */

  while (fracta < FRACTBIT)
    {				/* fraction too small */
      expa--;
      fracta <<= 1;
    }

  dpb (8, 8, expa, AC);
  dpb (35, 27, fracta, AC);

  if (sign)
    neg36 (AC, AC);

  if (expa > 255)
    setflags(PC_TRAP1 | PC_OV | PC_FOV);
  else if (expa < 0)
    setflags(PC_TRAP1 | PC_OV | PC_FOV | PC_FUF);
}

INST(fad, 0140)
{
  float_addsub (0140, ac, ea);
}

INST(fadm, 0142)
{
  float_addsub (0142, ac, ea);
}

INST(fadb, 0143)
{
  float_addsub (0143, ac, ea);
}

INST(fadr, 0144)
{
  float_addsub (0144, ac, ea);
}

INST(fadri, 0145)
{
  float_addsub (0145, ac, ea);
}

INST(fadrm, 0146)
{
  float_addsub (0146, ac, ea);
}

INST(fadrb, 0147)
{
  float_addsub (0147, ac, ea);
}

INST(fsb, 0150)
{
  float_addsub (0150, ac, ea);
}

INST(fsbm, 0152)
{
  float_addsub (0152, ac, ea);
}

INST(fsbb, 0153)
{
  float_addsub (0153, ac, ea);
}

INST(fsbr, 0154)
{
  float_addsub (0154, ac, ea);
}

INST(fsbri, 0155)
{
  float_addsub (0155, ac, ea);
}

INST(fsbrm, 0156)
{
  float_addsub (0156, ac, ea);
}

INST(fsbrb, 0157)
{
  float_addsub (0157, ac, ea);
}

INST(fmp, 0160)
{
  float_multiply (0160, ac, ea);
}

INST(fmpm, 0162)
{
  float_multiply (0162, ac, ea);
}

INST(fmpb, 0163)
{
  float_multiply (0163, ac, ea);
}

INST(fmpr, 0164)
{
  float_multiply (0164, ac, ea);
}

INST(fmpri, 0165)
{
  float_multiply (0165, ac, ea);
}

INST(fmprm, 0166)
{
  float_multiply (0166, ac, ea);
}

INST(fmprb, 0167)
{
  float_multiply (0167, ac, ea);
}

INST(fdv, 0170)
{
  float_divide (0170, ac, ea);
}

INST(fdvm, 0172)
{
  float_divide (0172, ac, ea);
}

INST(fdvb, 0173)
{
  float_divide (0173, ac, ea);
}

INST(fdvr, 0174)
{
  float_divide (0174, ac, ea);
}

INST(fdvri, 0175)
{
  float_divide (0175, ac, ea);
}

INST(fdvrm, 0176)
{
  float_divide (0176, ac, ea);
}

INST(fdvrb, 0177)
{
  float_divide (0177, ac, ea);
}

/* KL extended range double precision floating point.  */

INST(gfad, 0102)
{
  i_unimp (0102, ac, ea);
}

INST(gfsb, 0103)
{
  i_unimp (0103, ac, ea);
}

INST(gfmp, 0106)
{
  i_unimp (0106, ac, ea);
}

INST(gfdv, 0107)
{
  i_unimp (0107, ac, ea);
}

/* KA double precision floating point.  Usually implemented in software by the
   OS.  */

INST(ufa, 0130)
{
  i_uuo (0130, ac, ea);
}

INST(dfn, 0131)
{
  i_uuo (0131, ac, ea);
}

INST(fadl, 0141)
{
  i_uuo (0141, ac, ea);
}

INST(fsbl, 0151)
{
  i_uuo (0151, ac, ea);
}

INST(fmpl, 0161)
{
  i_uuo (0161, ac, ea);
}

INST(fdvl, 0171)
{
  i_uuo (0171, ac, ea);
}

/* Double precision integer instructions.  */

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

INST(dmul, 0116)
{
  word36 mem0, mem1;

  vfetch (ea, mem0);
  ea = increa (ea);
  vfetch (ea, mem1);

  if (!multiply_72 (AC, ACplus1, mem0, mem1, &AC, &ACplus1, &ACplus2, &ACplus3))
    setflags (PC_OV | PC_TRAP1);
}

INST(ddiv, 0117)
{
  word36 mem0, mem1;

  vfetch (ea, mem0);
  ea = increa (ea);
  vfetch (ea, mem1);

  if (!divide_72 (AC, ACplus1, ACplus2, ACplus3, mem0, mem1,
		  &AC, &ACplus1, &ACplus2, &ACplus3))
    if (pcflags & PC_USER)
      setflags (PC_TRAP1 | PC_OV | PC_NODIV);
    else
      setflags (PC_TRAP1 | PC_NODIV);
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

/* Single precision integer operations.  */

INST(imul, 0220)
{
  word36 mem;
  word36 prodhi, prodlo;

  vfetch(ea, mem);
  
  multiply(mem, AC, &prodhi, &prodlo);

  AC = prodlo;

  if ((ge36 (prodlo) && ne36(prodhi))
      || (lt36 (prodlo) && cne (prodhi, one36)))
    setflags (PC_TRAP1 | PC_OV);
}

INST(imuli, 0221)
{
  word36 tmp;
  word36 prodhi, prodlo;

  tmp = zero36;
  dpb(35, 18, ea, tmp);
  

  multiply(tmp, AC, &prodhi, &prodlo);

  AC = prodlo;

  if ((ge36 (prodlo) && ne36(prodhi))
      || (lt36 (prodlo) && cne (prodhi, one36)))
    setflags (PC_TRAP1 | PC_OV);
}

INST(imulm, 0222)
{
  word36 mem;
  word36 prodhi, prodlo;

  vfetch(ea, mem);
  
  multiply(mem, AC, &prodhi, &prodlo);

  vstore(ea, prodlo);

  if ((ge36 (prodlo) && ne36(prodhi))
      || (lt36 (prodlo) && cne (prodhi, one36)))
    setflags (PC_TRAP1 | PC_OV);
}

INST(imulb, 0223)
{
  word36 mem;
  word36 prodhi, prodlo;

  vfetch(ea, mem);
  
  multiply(mem, AC, &prodhi, &prodlo);

  vstore(ea, prodlo);

  AC = prodlo;

  if ((ge36 (prodlo) && ne36(prodhi))
      || (lt36 (prodlo) && cne (prodhi, one36)))
    setflags (PC_TRAP1 | PC_OV);
}

INST(mul, 0224)
{
  word36 mem;

  vfetch (ea, mem);
  
  if (!multiply (AC, mem, &AC, &ACplus1))
    setflags (PC_OV | PC_TRAP1);
}

INST(muli, 0225)
{
  word36 tmp;
  word36 prodhi, prodlo;

  tmp = zero36;
  dpb(35, 18, ea, tmp);
  
  multiply(AC, tmp, &prodhi, &prodlo);

  AC = prodhi;
  ACplus1 = prodlo;
}

INST(mulm, 0226)
{
  word36 mem;
  word36 prodhi, prodlo;

  vfetch (ea, mem);
  
  multiply(AC, mem, &prodhi, &prodlo);

  vstore (ea, prodhi);
}

INST(mulb, 0227)
{
  word36 mem;
  word36 prodhi, prodlo;

  vfetch (ea, mem);
  
  if (!multiply(AC, mem, &prodhi, &prodlo))
    pcflags |= PC_OV | PC_TRAP1;

  vstore (ea, prodhi);

  AC = prodhi;
  ACplus1 = prodlo;

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(idiv, 0230)
{
  word36 mem;
  word36 sign_ext;

  vfetch(ea, mem);
  
  if (ge36 (AC))
    sign_ext = zero36;
  else
    sign_ext = one36;

  if (!divide (sign_ext, AC, mem, &AC, &ACplus1))
    setflags (PC_TRAP1 | PC_OV | PC_NODIV);
}

INST(idivi, 0231)
{
  int tmp, dounsigned;
  int dividend, divisor, quotient, remainder; /* Must be signed */

/* We basically implement a cheap version of division by only handling 32
 * quantities for now...
 */

  tmp = ldb(4, 5, AC);

  dounsigned = 0;
  if (tmp == 01)
    dounsigned = 1;
  else if (tmp != 0 && tmp != 037)
    {
      word36 d;
      word36 sign_ext;

      if (ge36 (AC))
	sign_ext = zero36;
      else
	sign_ext = one36;

      d = zero36;
      dpb (35, 18, ea, d);

      if (!divide (sign_ext, AC, d, &AC, &ACplus1))
	setflags (PC_TRAP1 | PC_OV | PC_NODIV);

      return;
    }

  dividend = ldb(35, 32, AC);
  divisor = ea & HWORDMASK;

  if (divisor == 0)
    {
      setflags (PC_TRAP1 | PC_OV | PC_NODIV);
      return;
    }

  if (dounsigned)
    {
      quotient = (unsigned)dividend / (unsigned)divisor;
      remainder = (unsigned)dividend % (unsigned)divisor;
    }
  else
    {
      quotient = dividend / divisor;
      remainder = dividend % divisor;
    }

  dpb(35, 32, quotient, AC);
  if (quotient < 0 && !dounsigned)
    {
      dpb(3, 4, 017, AC);
    }
  else
    {
      dpb(3, 4, 0, AC);
    }

  dpb(35, 32, remainder, ACplus1);
  if (remainder < 0 && !dounsigned)
    {
      dpb(3, 4, 017, ACplus1);
    }
  else
    {
      dpb(3, 4, 0, ACplus1);
    }
}

INST(idivm, 0232)
{
  word36 mem;
  word36 sign_ext;
  word36 quotient, remainder;

  vfetch(ea, mem);
  
  if (ge36 (AC))
    sign_ext = zero36;
  else
    sign_ext = one36;

  if (!divide (sign_ext, AC, mem, &quotient, &remainder))
    {
      setflags (PC_TRAP1 | PC_OV | PC_NODIV);
      return;
    }

  vstore (ea, quotient);
}

INST(idivb, 0233)
{
  word36 mem;
  word36 sign_ext;
  word36 quotient, remainder;

  vfetch(ea, mem);
  
  if (ge36 (AC))
    sign_ext = zero36;
  else
    sign_ext = one36;

  if (!divide (sign_ext, AC, mem, &quotient, &remainder))
    {
      setflags (PC_TRAP1 | PC_OV | PC_NODIV);
      return;
    }

  vstore (ea, quotient);

  AC = quotient;
  ACplus1 = remainder;
}

INST(div, 0234)
{
  word36 mem;

  vfetch(ea, mem);

  if (!divide (AC, ACplus1, mem, &AC, &ACplus1))
    setflags (PC_TRAP1 | PC_OV | PC_NODIV);
}

INST(divi, 0235)
{
  unsigned long dividend, divisor, quotient, remainder;

/* We basically implement a cheap version of division by only handling 32
 * positive quantities for now...
 */

  if (ne36(AC) || ldb(3, 4, ACplus1))
    {
      word36 u;

      dpb (35, 18, ea, u);
      dpb (17, 18, 0, u);

      if (!divide (AC, ACplus1, u, &AC, &ACplus1))
	setflags (PC_TRAP1 | PC_OV | PC_NODIV);

      return;
    }

  dividend = ldb(35, 32, ACplus1);
  divisor = ea & HWORDMASK;

  quotient = dividend / divisor;
  remainder = dividend % divisor;

  dpb(35, 32, quotient, AC);
  dpb(35, 32, remainder, ACplus1);
}

INST(divm, 0236)
{
  word36 mem;
  word36 quotient, remainder;

  vfetch (ea, mem);

  if (!divide (AC, ACplus1, mem, &quotient, &remainder))
    {
      setflags (PC_TRAP1 | PC_OV | PC_NODIV);
      return;
    }

  vstore (ea, quotient);
}

INST(divb, 0237)
{
  word36 mem;
  word36 quotient, remainder;

  vfetch (ea, mem);

  if (!divide (AC, ACplus1, mem, &quotient, &remainder))
    {
      setflags (PC_TRAP1 | PC_OV | PC_NODIV);
      return;
    }

  vstore (ea, quotient);

  AC = quotient;
  ACplus1 = remainder;
}

INST(add, 0270)
{
  word36 mem;

  vfetch(ea, mem);

  add36_flags(AC, mem);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(addi, 0271)
{
  add36_imm_flags(AC, ea & HWORDMASK);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(addm, 0272)
{
  word36 mem;

  vfetch(ea, mem);

  add36_flags(mem, AC);

  vstore(ea, mem);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(addb, 0273)
{
  word36 mem;

  vfetch(ea, mem);

  add36_flags(mem, AC);

  vstore(ea, mem);

  AC = mem;

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(sub, 0274)
{
  word36 mem;

  vfetch(ea, mem);

  sub36_flags(AC, mem);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(subi, 0275)
{
  word36 tmp;

  tmp = zero36;

  dpb(35, 18, ea, tmp);		/* Create 0,,E */

  sub36_flags(AC, tmp);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(subm, 0276)
{
  word36 mem, tmp;

  vfetch(ea, mem);

  tmp = AC;

  sub36_flags(tmp, mem);

  vstore(ea, tmp);

  if (pcflags & PC_TRAP1)
    setflags (0);
}

INST(subb, 0277)
{
  word36 mem, tmp;

  vfetch(ea, mem);

  tmp = AC;

  sub36_flags(tmp, mem);

  vstore(ea, tmp);

  AC = tmp;

  if (pcflags & PC_TRAP1)
    setflags (0);
}
