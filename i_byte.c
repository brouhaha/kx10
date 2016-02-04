/* Byte instructions for kx10, the PDP-10 emulator.
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

/* This file contains the byte pointer manipulation instructions.
 */

#include "pdp10.h"

/* This table maps from One Word Global byte Pointers to the equivalent p
   and s values.  */

static struct owgp
{
  unsigned int pos;		/* Value of P field */
  unsigned int size;		/* Value of S field */
  unsigned int next;		/* Next owgp after incrementing this one */
  unsigned int nextpos;		/* Next pos after incrementing */
  unsigned int inc;		/* 0 or 1 if increment goes to next word */
}
owgp[] =
{
/* 6 bit bytes */

  36, 6, 38, 30, 0,		/* 37 */
  30, 6, 39, 24, 0,		/* 38 */
  24, 6, 40, 18, 0,		/* 39 */
  18, 6, 41, 12, 0,		/* 40 */
  12, 6, 42, 6, 0,		/* 41 */
  6, 6,	43, 0, 0,		/* 42 */
  0, 6,	38, 30, 1,		/* 43 */

/* 8 bit bytes */

  36, 8, 45, 28, 0,		/* 44 */
  28, 8, 46, 20, 0,		/* 45 */
  20, 8, 47, 12, 0,		/* 46 */
  12, 8, 48, 4, 0,		/* 47 */
  4, 8,	45, 28, 1,		/* 48 */

/* 7 bit bytes */

  36, 7, 50, 29, 0,		/* 49 */
  29, 7, 51, 22, 0,		/* 50 */
  22, 7, 52, 15, 0,		/* 51 */
  15, 7, 53, 8, 0,		/* 52 */
  8, 7,	54, 1, 0,		/* 53 */
  1, 7,	50, 29, 1,		/* 54 */

/* 9 bit bytes */

  36, 9, 56, 27, 0,		/* 55 */
  27, 9, 57, 18, 0,		/* 56 */
  18, 9, 58, 9, 0,		/* 57 */
  9, 9,	59, 0, 0,		/* 58 */
  0, 9,	56, 27, 1,		/* 59 */

/* 18 bit bytes */

  36, 18, 61, 18, 0,		/* 60 */
  18, 18, 62, 0, 0,		/* 61 */
  0, 18, 61, 18, 1,		/* 62 */

/* 36 bit bytes */

  0, 36, 63, 0, 1,		/* 63 (KL doesn't really have this, but I
				   think it really ought to! -Stu */
};

/* Byte-pointer parsing and incrementing macros.  These macros generate inline
   code to increment or extract pos, size and ea from byte pointers.  These
   operations are so similar and intertwined that it makes sense to do this as
   a set of macros rather than an inline function, which would introduce
   certain inefficiencies because of the need to return as many as four or five
   values which would result from the parsing process.  */

/* The byte-pointer code for ILDB/IDPB is actually split into two cases.  One
   for regular (and owgs), and another for two-word pointers.  The reason for
   this is that it saves the common case from having to do any tests to handle
   differences between one-word and two-word pointers.  */

#define increment_bp(EA, BP, RTN_2WG) \
  FONDLE_BP ((EA), local_ea2, 0, 1, 0, (BP), local_bp2, local_pos, local_size, local_bpea, RTN_2WG)
#define loadstore_bp(EA, BP, POS, SIZE, BPEA) \
  FONDLE_BP ((EA), local_ea2, 0, 0, 1, (BP), local_bp2, (POS), (SIZE), (BPEA), ibp)
#define increment_loadstore_bp(EA, BP, POS, SIZE, BPEA, RTN_2WG) \
  FONDLE_BP ((EA), local_ea2, 0, 1, 1, (BP), local_bp2, (POS), (SIZE), (BPEA), RTN_2WG)

#define increment_bp_2wg(EA, EA2, BP, BP2, POS, SIZE) \
  FONDLE_BP ((EA), (EA2), 1, 1, 0, (BP), (BP2), (POS), (SIZE), local_bpea, ibp)
#define loadstore_bp_2wg(EA, EA2, BP, BP2, POS, SIZE, BPEA) \
  FONDLE_BP ((EA), (EA2), 1, 0, 1, (BP), (BP2), (POS), (SIZE), (BPEA), ibp)
#define increment_loadstore_bp_2wg(EA, EA2, BP, BP2, POS, SIZE, BPEA) \
  FONDLE_BP ((EA), (EA2), 1, 1, 1, (BP), (BP2), (POS), (SIZE), (BPEA), ibp)

/* This big, ugly macro contains all knowledge of byte-pointer parsing and
   incrementing.  It should not be called directly (see macros above).  It
   relies heavily on the fact that the compiler will eliminate tests for
   constants, and also eliminate the dead code that would result.  TWGBP,
   INCREMENT, and LOADSTORE must be compile-time constants for this to work
   right.

   TWGBP indicates that we are compiling code that will run in a two-word
   global (_2wgp) routine.  This flag eliminates certain parsing code, and
   unnecessary tests (such as the bit 12 test) which have already been done
   by the time the _2wgp routine is called.

   INCREMENT turns on the byte-pointer incrementing code.  It's used by IBP,
   ILDB and IDPB.

   LOADSTORE enables the ea calc, and adjusts POS and SIZE to guarantee they
   are in range for byte load and store operations.  ILDB/IDPB/LDB/DPB turn
   on this flag.
*/

#define FONDLE_BP(EA, EA2, TWGBP, INCREMENT, LOADSTORE, BP, BP2, POS, SIZE, BPEA, RTN_2WG)	\
{									\
  int local_pos, local_size;						\
  addr10 local_bpea;							\
  addr10 local_ea2;							\
  word36 local_bp2;							\
									\
  if (!TWGBP)								\
    {									\
      vfetch (EA, BP);		/* Fetch the byte pointer */		\
      POS = ldb (5, 6, BP);		/* Get the P field */		\
    }									\
  else									\
    {									\
      (EA2) = increa (EA);						\
      vfetch ((EA2), BP2);						\
    }									\
									\
  if (TWGBP || POS <= 36 || pcsection == 0) /* Simple case */		\
    {									\
      if (!TWGBP)							\
        SIZE = ldb (11, 6, BP);						\
									\
      if (TWGBP || !biton (12, BP) || pcsection == 0)			\
	{			/* Regular byte pointer */		\
	  if (INCREMENT)						\
	    {								\
	      POS -= SIZE;						\
									\
	      if (POS < 0)						\
		{		/* Went into next word, inc Y */	\
		  POS = (36 - SIZE) & 077;				\
		  if (!TWGBP)						\
		    {							\
		      dpb (35, 18, lower18 (BP) + 1, BP);		\
		    }							\
		  else							\
		    {							\
		      if (biton (0, (BP2)))				\
			{						\
			  dpb (35, 18, lower18 (BP2) + 1, BP2);		\
			}						\
		      else						\
			{						\
			  dpb (35, 30, ldb (35, 30, BP2) + 1, BP2);	\
			}						\
		    }							\
		}							\
									\
	      dpb (5, 6, POS, BP);					\
	    }								\
	  if (LOADSTORE)						\
	    if (!TWGBP)							\
	      BPEA = ieacalc (BP, pcsection); /* Calc the ea of the byte pointer */ \
	    else							\
	      BPEA = efiw_eacalc (BP2, pcsection); /* Calc the ea of the byte pointer */ \
	}								\
      else			/* Two-word global byte pointer */	\
	if (INCREMENT)							\
	  {								\
	    CONCAT (RTN_2WG,_2wgp) (ac, EA, BP, POS, SIZE);		\
	    return;							\
	  }								\
    }									\
  else									\
    {									\
      SIZE = owgp[POS - 37].size;					\
      if (INCREMENT)							\
	{								\
	  add36const (BP, owgp[POS - 37].inc);				\
	  dpb (5, 6, owgp[POS - 37].next, BP);				\
	  POS = owgp[POS - 37].nextpos;					\
	}								\
      else								\
	POS = owgp[POS - 37].pos;					\
									\
      if (LOADSTORE)							\
	BPEA = ldb (35, 30, BP) | GLOBAL;				\
    }									\
									\
  if (LOADSTORE)							\
    {									\
      if (POS + SIZE > 36)						\
	if (POS < 36)							\
	  SIZE = 36 - POS;						\
	else								\
	  {								\
	    POS = 0;							\
	    SIZE = 0;							\
	  }								\
    }									\
}

static void ibp_2wgp PARAMS ((int ac, addr10 ea, word36 bp, int pos, int size));
static void ildb_2wgp PARAMS ((int ac, addr10 ea, word36 bp, int pos, int size));
static void idpb_2wgp PARAMS ((int ac, addr10 ea, word36 bp, int pos, int size));
static word36 internal_ldb PARAMS ((int pos, int size, addr10 ea));
static word36 internal_ildb PARAMS ((word36 *bpp, addr10 ea));
static void internal_idpb PARAMS ((word36 byte1, word36 *bpp, addr10 ea));
static int internal_bpsize PARAMS ((word36 bp));
void compare_strings PARAMS ((int opcode, int ac, addr10 ea));
void movst PARAMS ((int opcode, int ac, addr10 ea, word36 e0));

static void
ibp_2wgp (ac, ea, bp, pos, size)
     int ac;
     addr10 ea;
     word36 bp;
     int pos;
     int size;
{
  word36 bp2;
  addr10 ea2;

  increment_bp_2wg (ea, ea2, bp, bp2, pos, size);

  vstore(ea, bp);

  vstore(ea2, bp2);
}

INST(ibp, 0133)
{
  int pos, size, tmp;
  word36 bp;

  if (ac == 0)
    {				/* ibp */
      increment_bp (ea, bp, ibp);

      vstore (ea, bp);
    }
  else
    {				/* adjbp */
      int alignment;
      int bpw;
      int curbyt;
      word36 tmp;
      int y;
      int newcurbyt;

      vfetch(ea, bp);		/* Fetch the byte pointer */

      pos = ldb(5, 6, bp);		/* Get the P field */

      if (biton (12, bp))
	{
	  printf ("adjbp of 2wgp not yet supported.  bp = %o,,%o, inc = %o,,%o, pc = %o,,%o\r\n",
		  upper18(bp), lower18(bp), upper18(AC), lower18(AC),
		  (pcflags << 5) | (pcsection >> 18), pc);
#if 0
	  genunimp();
#endif
	  return;
	}

      if (pos > 36)
	{
	  if (ceq (AC, one36))
	    {
	      /* Handle the case of decrementing OWG by 1 inline since it's so common. */
	      switch (pos)
		{
		  /* Not too sure how to handle the pre-decrement cases
		     (ie: p=36).  For now, we treat them like p=0.  */
		case 37:	/* 6 bit byte pointers */
		case 38:
		  pos += 5;
		  decr36 (bp);
		  break;
		case 39:
		case 40:
		case 41:
		case 42:
		case 43:
		  pos--;
		  break;

		case 44:	/* 8 bit byte pointers */
		case 45:
		  pos += 3;
		  decr36 (bp);
		  break;
		case 46:
		case 47:
		case 48:
		  pos--;
		  break;

		case 49:	/* 7 bit byte pointers */
		case 50:
		  pos += 4;
		  decr36 (bp);
		  break;
		case 51:
		case 52:
		case 53:
		case 54:
		  pos--;
		  break;

		case 55:	/* 9 bit byte pointers */
		case 56:
		  pos += 3;
		  decr36 (bp);
		  break;
		case 57:
		case 58:
		case 59:
		  pos--;
		  break;

		case 60:	/* 18 bit byte pointers */
		case 61:
		  pos += 1;
		  decr36 (bp);
		  break;
		case 62:
		  pos--;
		  break;

		case 63:	/* 36 bit byte pointers */
		  decr36 (bp);
		  break;
		}
	      dpb (5, 6, pos, bp);
	    }
	  else
	    {			/* adjbp of owgp by something other than -1 */
	      int i;

	      size = owgp[pos - 37].size;
	      pos = owgp[pos - 37].pos;

	      alignment = pos % size;
	      bpw = (36 - alignment) / size;
	      curbyt = (36 - pos) / size - 1;

	      tmp = AC;
	      add36const (tmp, curbyt);

	      curbyt = ldb (35, 32, tmp);
	      y = curbyt / bpw + lower18 (bp);
	      newcurbyt = curbyt % bpw;
	      if (curbyt < 0 && newcurbyt != 0)
		{
		  y--;
		  newcurbyt += bpw;
		}

	      pos = ((bpw - 1) - newcurbyt) * size + alignment;

	      for (i = 37; i < 64; i++)
		{
		  if (owgp[i - 37].size == size
		      && owgp[i - 37].pos == pos)
		    {
		      pos = i;
		      break;
		    }
		}
	      if (i == 64)
		{
		  printf ("Couldn't convert bp back to owgp.  bp = %o,,%o, inc = %o,,%o, pc = %o,,%o\r\n",
			  upper18(bp), lower18(bp), upper18(AC), lower18(AC),
			  (pcflags << 5) | (pcsection >> 18), pc);
		}

	      dpb (5, 6, pos, bp);
	      dpb (35, 18, y, bp);
	    }
	}
      else
	{			/* Normal bp */
	  size = ldb(11, 6, bp);

	  if (size != 0)
	    {
	      alignment = pos % size;
	      bpw = (36 - alignment) / size;
	      curbyt = (36 - pos) / size - 1;

	      tmp = AC;
	      add36const (tmp, curbyt);

	      curbyt = ldb (35, 32, tmp);
	      y = curbyt / bpw + lower18 (bp);
	      newcurbyt = curbyt % bpw;
	      if (curbyt < 0 && newcurbyt != 0)
		{
		  y--;
		  newcurbyt += bpw;
		}

	      pos = ((bpw - 1) - newcurbyt) * size + alignment;
	      dpb (5, 6, pos, bp);
	      dpb (35, 18, y, bp);
	    }
	}

      AC = bp;
      return;
    }
}

static void
ildb_2wgp (ac, ea, bp, pos, size)
     int ac;
     addr10 ea;
     word36 bp;
     int pos;
     int size;
{
  word36 mem, bp2;
  addr10 ea2;
  addr10 bpea;

  increment_loadstore_bp_2wg (ea, ea2, bp, bp2, pos, size, bpea);

/* Now, use it */

  vfetch_1 (bpea, mem);		/* Fetch the object of the byte pointer */

  rshift36 (pos, mem);		/* Shift right  */
  mask36 (size, mem);		/* Mask out unwanted bits */

/* Note that we store the byte pointer AFTER loading the byte.  This way, if we
   fault while reading the byte, the byte pointer in memory is left
   undisturbed, and we can avoid having to use first-part-done and all the crap
   that entails. */

  vstore (ea, bp);
  vstore (ea2, bp2);

  AC = mem;
}

INST(ildb, 0134)
{
  int pos, size;
  addr10 bpea;
  word36 bp, mem;

  increment_loadstore_bp (ea, bp, pos, size, bpea, ildb);

/* Now, use it */

  vfetch_1 (bpea, mem);		/* Fetch the object of the byte pointer */

  rshift36 (pos, mem);		/* Shift right  */
  mask36 (size, mem);		/* Mask out unwanted bits */

/* Note that we store the byte pointer AFTER loading the byte.  This way, if we
   fault while reading the byte, the byte pointer in memory is left
   undisturbed, and we can avoid having to use first-part-done and all the crap
   that entails. */

  vstore (ea, bp);

  AC = mem;
}

INST(ldb, 0135)
{
  int pos, size;
  word36 bp, mem;

  loadstore_bp (ea, bp, pos, size, ea);

  vfetch_1 (ea, mem);		/* Fetch the object of the byte pointer */

  rshift36 (pos, mem);		/* Shift right  */
  mask36 (size, mem);		/* Mask out unwanted bits */

  AC = mem;
}

static void
idpb_2wgp (ac, ea, bp, pos, size)
     int ac;
     addr10 ea;
     word36 bp;
     int pos;
     int size;
{
  int tmp;
  addr10 ea2;
  word36 mem, bp2;
  addr10 bpea;

  increment_loadstore_bp_2wg (ea, ea2, bp, bp2, pos, size, bpea);

/* Now, use it */

  vfetch_1(bpea, mem);		/* Fetch the object of the byte pointer */

  if (size <= 32)
    {				/* Easy case, field fits in a 32 bit word */
      tmp = ldb (35, size, AC);	/* Fetch the byte from the ac*/
      dpb (35 - pos, size, tmp, mem); /* Store it into the word */
    }
  else
    {				/* A little tougher, gotta do this in pieces */
      tmp = ldb (35, 32, AC);	/* Fetch the lower part from the ac */
      dpb (35 - pos, 32, tmp, mem); /* Store it into the word */

      tmp = ldb (3, size - 32, AC); /* Get the upper part */
      dpb (3 - pos, size - 32, tmp, mem); /* Store the upper part */
    }

  vstore_1(bpea, mem);		/* Store the word back into memory */

/* Note that we store the byte pointer AFTER storing the byte.  This way, if we
   fault while writing the byte, the byte pointer in memory is left
   undisturbed, and we can avoid having to use first-part-done and all the crap
   that entails. */

  vstore(ea, bp);
  vstore(ea2, bp2);
}

INST(idpb, 0136)
{
  int pos, size, tmp;
  addr10 bpea;
  word36 bp, mem;

  increment_loadstore_bp (ea, bp, pos, size, bpea, idpb);

/* Now, use it */

  vfetch_1 (bpea, mem);		/* Fetch the object of the byte pointer */

  if (size <= 32)
    {				/* Easy case, field fits in a 32 bit word */
      tmp = ldb (35, size, AC);	/* Fetch the byte from the ac*/
      dpb (35 - pos, size, tmp, mem); /* Store it into the word */
    }
  else
    {				/* A little tougher, gotta do this in pieces */
      tmp = ldb (35, 32, AC);	/* Fetch the lower part from the ac */
      dpb (35 - pos, 32, tmp, mem); /* Store it into the word */

      tmp = ldb (3, size - 32, AC); /* Get the upper part */
      dpb (3 - pos, size - 32, tmp, mem); /* Store the upper part */
    }

  vstore_1 (bpea, mem);		/* Store the word back into memory */

/* Note that we store the byte pointer AFTER storing the byte.  This way, if we
   fault while writing the byte, the byte pointer in memory is left
   undisturbed, and we can avoid having to use first-part-done and all the crap
   that entails. */

  vstore (ea, bp);
}

INST(dpb, 0137)
{
  int pos, size, tmp;
  word36 bp, mem;

  loadstore_bp (ea, bp, pos, size, ea);

  vfetch_1 (ea, mem);		/* Fetch the object of the byte pointer */

  if (size <= 32)
    {				/* Easy case, field fits in a 32 bit word */
      tmp = ldb (35, size, AC);	/* Fetch the byte from the ac*/
      dpb (35 - pos, size, tmp, mem); /* Store it into the word */
    }
  else
    {				/* A little tougher, gotta do this in pieces */
      tmp = ldb (35, 32, AC);	/* Fetch the lower part from the ac */
      dpb (35 - pos, 32, tmp, mem); /* Store it into the word */

      tmp = ldb (3, size - 32, AC); /* Get the upper part */
      dpb (3 - pos, size - 32, tmp, mem); /* Store the upper part */
    }

  vstore_1 (ea, mem);		/* Store the word back into memory */
}

static word36
internal_ldb (pos, size, ea)
     int pos;
     int size;
     addr10 ea;
{
  int tmp;
  word36 mem;
  word36 retval;

  vfetch (ea, mem);		/* Fetch the object of the byte pointer */

  if (pos + size > 36)
    if (pos < 36)
      size = 36 - pos;
    else
      return zero36;

  if (size <= 32)
    {				/* Easy case, field fits in a 32 bit word */
      tmp = ldb (35 - pos, size, mem); /* Fetch the byte */
      dpb (35, 32, tmp, retval); /* Store into the ac */
      dpb (3, 4, 0, retval);	/* Clear the top few bits */
    }
  else
    {				/* A little tougher, gotta do this in pieces */
      tmp = ldb (35 - pos, 32, mem); /* Get the low order part */
      dpb (35, 32, tmp, retval); /* Store the low part */
      tmp = ldb (3 - pos, size - 32, mem); /* Get the upper part */
      dpb (3, 4, tmp, retval);	/* Store the upper part */
    }

  return retval;
}

static word36
internal_ildb (bpp, ea)
     word36 *bpp;
     addr10 ea;
{
  int pos, size, tmp;
  addr10 bpea;
  word36 mem, retval;

  pos = ldb(5, 6, bpp[0]);	/* Get the P field */

  if (pos <= 36)
    {
      size = ldb(11, 6, bpp[0]);

      pos -= size;

      if (pos < 0)
	{			/* Went into next word, inc Y */
	  pos = 36 - size;

	  if (biton(12, bpp[0]) && pcsection != 0)
	    {
	      if (lt36 (bpp[1]))
		{
		  tmp = ldb (35, 30, bpp[1]) + 1;
		  dpb (35, 18, tmp, bpp[1]);
		}
	      else
		{
		  tmp = lower18(bpp[1]) + 1;
		  dpb(35, 18, tmp, bpp[1]);
		}
	    }
	  else
	    {
	      tmp = lower18(bpp[0]) + 1;
	      dpb(35, 18, tmp, bpp[0]);
	    }
	}

      dpb(5, 6, pos, bpp[0]);

      if (biton(12, bpp[0]) && pcsection != 0)
	bpea = efiw_eacalc(bpp[1], pcsection); /* Calc the ea of the byte pointer */
      else
	bpea = ieacalc(bpp[0], pcsection); /* Calc the ea of the byte pointer */
    }
  else
    {
      size = owgp[pos - 37].size;
      add36const (bpp[0], owgp[pos - 37].inc);
      dpb (5, 6, owgp[pos - 37].next, bpp[0]);
      pos = owgp[pos - 37].nextpos;
      bpea = ldb (35, 30, bpp[0]) | GLOBAL;
    }

/* Now, use it */

  vfetch_1(bpea, mem);		/* Fetch the object of the byte pointer */

  if (pos + size > 36)
    if (pos < 36)
      size = 36 - pos;
    else
      return zero36;

  if (size <= 32)
    {				/* Easy case, field fits in a 32 bit word */
      tmp = ldb (35 - pos, size, mem); /* Fetch the byte */
      dpb (35, 32, tmp, retval); /* Store into the ac */
      dpb (3, 4, 0, retval);	/* Clear the top few bits */
    }
  else 
    {				/* A little tougher, gotta do this in pieces */
      tmp = ldb (35 - pos, 32, mem); /* Get the low order part */
      dpb (35, 32, tmp, retval); /* Store the low part */
      tmp = ldb (3 - pos, size - 32, mem); /* Get the upper part */
      dpb (3, 4, tmp, retval);	/* Store the upper part */
    }

  return retval;
}

static void
internal_idpb (byte1, bpp, ea)
     word36 byte1;
     word36 *bpp;
     addr10 ea;
{
  int pos, size, tmp;
  addr10 bpea;
  word36 mem;

  pos = ldb(5, 6, bpp[0]);	/* Get the P field */

  if (pos <= 36)
    {
      size = ldb(11, 6, bpp[0]);

      pos -= size;

      if (pos < 0)
	{			/* Went into next word, inc Y */
	  pos = 36 - size;

	  if (biton(12, bpp[0]) && pcsection != 0)
	    {
	      if (lt36 (bpp[1]))
		{
		  tmp = ldb (35, 30, bpp[1]) + 1;
		  dpb (35, 18, tmp, bpp[1]);
		}
	      else
		{
		  tmp = lower18(bpp[1]) + 1;
		  dpb(35, 18, tmp, bpp[1]);
		}
	    }
	  else
	    {
	      tmp = lower18(bpp[0]) + 1;
	      dpb(35, 18, tmp, bpp[0]);
	    }
	}

      dpb(5, 6, pos, bpp[0]);

      if (biton(12, bpp[0]) && pcsection != 0)
	bpea = efiw_eacalc(bpp[1], pcsection); /* Calc the ea of the byte pointer */
      else
	bpea = ieacalc(bpp[0], pcsection); /* Calc the ea of the byte pointer */
    }
  else
    {
      size = owgp[pos - 37].size;
      add36const (bpp[0], owgp[pos - 37].inc);
      dpb (5, 6, owgp[pos - 37].next, bpp[0]);
      pos = owgp[pos - 37].nextpos;
      bpea = ldb (35, 30, bpp[0]) | GLOBAL;
    }

/* Now, use it */

  vfetch_1(bpea, mem);		/* Fetch the object of the byte pointer */

  if (pos + size > 36)
    if (pos < 36)
      size = 36 - pos;
    else
      return;

  if (size <= 32)
    {				/* Easy case, field fits in a 32 bit word */
      tmp = ldb (35, size, byte1); /* Fetch the byte */
      dpb (35 - pos, size, tmp, mem); /* Store it into the word */
    }
  else 
    {				/* A little tougher, gotta do this in pieces */
      tmp = ldb (35, 32, byte1); /* Fetch the lower part */
      dpb (35 - pos, 32, tmp, mem); /* Store it into the word */

      tmp = ldb (3, size - 32, byte1); /* Get the upper part */
      dpb (3 - pos, size - 32, tmp, mem); /* Store the upper part */
    }

  vstore_1 (bpea, mem);		/* Store the word back into memory */
}

static int
internal_bpsize (bp)
     word36 bp;
{
  int pos;
  int size;

  pos = ldb(5, 6, bp);		/* Get the P field */

  if (pos <= 36)
    size = ldb(11, 6, bp);
  else
    size = owgp[pos - 37].size;

  return size;
}

/* String comparison instructions under extend.  cmpsl, cmpsle, cmpse, cmpsn,
   cmpsg, cmpsge.  */

/* Note that this procedure is not really optimal w/respect to page faults, as
   it restarts the entire comparison if a fault occurs while fetching a byte.
   On the other hand, most of the time the strings can be accessed without a
   page fault, so we use some shortcuts which avoid updating user-visible state
   until the end of the comparison.  */

void
compare_strings (opcode, ac, ea)
     int opcode;
     int ac;
     addr10 ea;
{
  unsigned int src_len, dst_len;
  word36 src_ptr[2], dst_ptr[2];
  word36 byte1, byte2;
  int size;
  word36 fill;

  if (ldb (8, 9, AC) != 0
      || ldb (8, 9, ACplus3) != 0)
    {
      i_uuo (0123, ac, ea);
      return;
    }

  src_len = ldb (35, 27, AC);
  src_ptr[0] = ACplus1;
  src_ptr[1] = ACplus2;
  dst_len = ldb (35, 27, ACplus3);
  dst_ptr[0] = ACplus4;
  dst_ptr[1] = ACplus5;

  /* If strings are of unequal length, then we need to fetch a fill byte for
     the shorter string.  */

  if (src_len != dst_len)
    {
      ea = increa (ea);		/* Point to fill1 */

      if (src_len < dst_len)
	size = internal_bpsize (src_ptr[0]);
      else
	{
	  size = internal_bpsize (dst_ptr[0]);
	  ea = increa (ea);	/* Point to fill2 */
	}
      fill = internal_ldb (0, size, ea); /* Fetch fill byte */
    }

  while (src_len > 0 || dst_len > 0)
    {
      if (src_len > 0)
	{
	  byte1 = internal_ildb (src_ptr, ea);
	  src_len--;
	}
      else
	byte1 = fill;

      if (dst_len > 0)
	{
	  byte2 = internal_ildb (dst_ptr, ea);
	  dst_len--;
	}
      else
	byte2 = fill;

      if (ceq (byte1, byte2))
	continue;

      /* Found an inequality, see if it meets the condition */

      switch (opcode)
	{
	case 001:		/* cmpsl */
	case 003:		/* cmpsle */
	  if (cltu (byte1, byte2))
	    incrpc;
	  break;
	case 006:		/* cmpsn */
	  incrpc;
	  break;
	case 005:		/* cmpsge */
	case 007:		/* cmpsg */
	  if (cltu (byte2, byte1))
	    incrpc;
	  break;
	}
      goto finish;
    }

  /* If we get here, then strings are identical, see if that's what we're
     looking for.  */

  switch (opcode)
    {
    case 002:			/* cmpse */
    case 003:			/* cmpsle */
    case 005:			/* cmpsge */
      incrpc;
    }

 finish:
  dpb (35, 27, src_len, AC);
  ACplus1 = src_ptr[0];
  ACplus2 = src_ptr[1];
  dpb (35, 27, dst_len, ACplus3);
  ACplus4 = dst_ptr[0];
  ACplus5 = dst_ptr[1];
}

/* This implements most of the string movement instructions.  movslj, movsrj,
   movso, movst, etc.. */

void
move_string (opcode, ac, ea, e0)
     int opcode;
     int ac;
     addr10 ea;
     word36 e0;
{
  word36 src_ptr[2], dst_ptr[2];
  int src_len;
  int dst_len;
  addr10 table_ea;
  word36 movso_mask;
  int movso_offset;
  int flags;
  int i;
  int skip;

#define S_FLAG 0400
#define N_FLAG 0200
#define M_FLAG 0100

  skip = 1;			/* Assume source <= dest */

  if ((opcode == 015 ? ldb (8, 6, AC) : ldb (8, 9, AC)) != 0
      || ldb (8, 9, ACplus3) != 0)
    {
      i_uuo (0123, ac, ea);	/* Length out of bounds */
      return;
    }

  src_len = ldb (35, 27, AC);
  src_ptr[0] = ACplus1;
  src_ptr[1] = ACplus2;
  dst_len = ldb (35, 27, ACplus3);
  dst_ptr[0] = ACplus4;
  dst_ptr[1] = ACplus5;

  switch (opcode)
    {
    case 015:			/* movst */
      table_ea = ieacalc (e0, ea & SECTMASK);
      flags = ldb (8, 9, AC);
      break;
    case 014:			/* movso */
      {
	int dst_size;

	movso_offset = ieacalc (e0, ea & SECTMASK) & HWORDMASK;

	movso_offset = (movso_offset ^ 0400000) - 0400000; /* Sign extend */

	dst_size = internal_bpsize (dst_ptr[0]);
	movso_mask = one36;
	lshift36 (dst_size, movso_mask);
      }
      break;
    case 017:			/* movsrj */
      if (dst_len > src_len)
	{
	  word36 fill;

	  /* Source is shorter than dest.  Fill on left.  */

	  ea = increa (ea);	/* Point to fill */

	  vfetch (ea, fill);	/* Fetch fill byte */

	  for (i = dst_len - src_len; i > 0; i--)
	    internal_idpb (fill, dst_ptr, ea);

	  dst_len = src_len;	/* Now lengths are same */
	}
      else if (dst_len < src_len)
	{
	  /* Source is longer than dest.  Cut off left part of string. */

	  for (i = src_len - dst_len; i > 0; i--)
	    internal_ildb (src_ptr, ea);

	  src_len = dst_len;	/* Now lengths are the same */
	}
      break;
    }

/* Copy common part of string */

  if (opcode != 015)
    {				/* movslj, movsrj, movso */

      i = min (src_len, dst_len); /* Length of copy */
      src_len -= i;
      dst_len -= i;

      for (; i > 0; i--)
	{
	  word36 byte_tmp;

	  byte_tmp = internal_ildb (src_ptr, ea);
	  if (opcode == 014)
	    {			/* movso */
	      add36const (byte_tmp, movso_offset);
	      if (testbits36 (byte_tmp, movso_mask))
		{		/* Oversize byte */
		  src_len += i - 1;
		  dst_len += i;
		  skip = 0;
		  goto finish;
		}
	    }
	  internal_idpb (byte_tmp, dst_ptr, ea);
	}
    }
  else
    while (src_len > 0 && dst_len > 0) /* movst copy loop */
      {
	word36 byte_tmp;

	byte_tmp = internal_ildb (src_ptr, ea);
	src_len--;

	if (opcode == 015)	/* movst */
	  {
	    int c;
	    int op;

	    c = ldb (35, 32, byte_tmp);

	    vfetch (table_ea + c/2, byte_tmp);

	    if (c & 1)
	      {
		op = ldb (20, 3, byte_tmp);
		c = ldb (35, 12, byte_tmp);
	      }
	    else
	      {
		op = ldb (2, 3, byte_tmp);
		c = ldb (17, 12, byte_tmp);
	      }

	    switch (op)
	      {
	      case 0:
		if (!(flags & S_FLAG))
		  continue;
		break;
	      case 1:
		skip = 0;
		goto finish;
	      case 2:
		flags &= ~M_FLAG;
		if (!(flags & S_FLAG))
		  continue;
		break;
	      case 3:
		flags |= M_FLAG;
		if (!(flags & S_FLAG))
		  continue;
		break;
	      case 4:
		flags |= S_FLAG | N_FLAG;
		break;
	      case 5:
		flags |= N_FLAG;
		skip = 0;
		goto finish;
	      case 6:
		flags = S_FLAG | N_FLAG;
		break;
	      case 7:
		flags = S_FLAG | N_FLAG | M_FLAG;
		break;
	      }
	  }

	dpb (23, 24, 0, byte_tmp);
	internal_idpb (byte_tmp, dst_ptr, ea);
	dst_len--;
      }

  if  (opcode != 017)		/* Not movsrj */
    if (dst_len > 0)
      {
	word36 fill;

	/* Source is shorter than dest.  Fill on right.  */

	ea = increa (ea);	/* Point to fill */

	vfetch (ea, fill);	/* Fetch fill byte */

	for (; dst_len > 0; dst_len--)
	  internal_idpb (fill, dst_ptr, ea);
      }
    else if (src_len > 0)
      skip = 0;			/* Source too long, don't skip */

 finish:
  dpb (35, 27, src_len, AC);
  if (opcode == 015)		/* movst */
    {
      dpb (8, 9, flags, AC);
    }
  ACplus1 = src_ptr[0];
  ACplus2 = src_ptr[1];
  dpb (35, 27, dst_len, ACplus3);
  ACplus4 = dst_ptr[0];
  ACplus5 = dst_ptr[1];

  if (skip)
    incrpc;
}
