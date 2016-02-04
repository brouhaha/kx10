/* Definitions for kx10, the PDP-10 emulator.
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

/* This file contains all of the generic pdp10 data definitions.  It also
   currently contains host specific definitions.
 */

#include <config.h>
#include <sys/types.h>
#include <signal.h>
#include <setjmp.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#ifndef SA_RESTART
  /* Needed for SunOS 4.1.x */
#define SA_RESTART 0
#endif

sigjmp_buf to_main;

/* #define PCHIST */			/* PC history code */

/* #define DISK_PROFILE		/* Disk I/O profiling code */

#ifdef DISK_PROFILE
void print_io_times PARAMS ((void));
#else
#define myread read
#define mywrite write

#define print_io_times()
#endif

/* #define PC_PROFILE */		/* Inst fetch profiling code */

#ifdef PC_PROFILE
void print_pccounts PARAMS ((void));
#else
#define print_pccounts()
#endif

typedef unsigned int addr10;
typedef unsigned int page10;

/* Host system dependant */

typedef union
{
  double d;			/* Force alignment */
  struct
    {
      unsigned int whi;		/* Upper 18 bits */
      unsigned int wlo;		/* Lower 18 bits */
    } w;
} word36;

#define hi w.whi
#define lo w.wlo

#ifndef MEMPAGES
#define MEMPAGES 3000
#endif
word36 memarray[MEMPAGES * 512]; /* Main memory */
addr10 pc;			/* Program counter, low 18 bits */
addr10 pcsection;		/* PC section number, shifted left 18 */
addr10 prev_pcsection;		/* Same as above, but for prev section */
int pcflags;			/* PC flags, shifted into the right place */
int trapflags;			/* Trap 0 and Trap 1 */
#define PC_OV	010000		/* Overflow */
#define PC_CRY0	004000		/* Carry 0 */
#define PC_CRY1	002000		/* Carry 1 */
#if 0
extern int set_cry0(), set_cry1();
#define PC_CRY0 set_cry0()
#define PC_CRY1 set_cry1()
#endif
#define PC_FOV	001000		/* Floating overflow */
#define PC_FPD  000400		/* First part done */
#define PC_USER 000200		/* User mode */
#define PC_PCU  000100		/* Prev context user mode */
#define PC_UIO  000100		/* User I/O */
#define PC_PUBLIC  040		/* Public */
#define PC_AFI     020		/* Address failure inhibit */
#define PC_TRAP2   010		/* Trap 2 */
#define PC_TRAP1    04		/* Trap 1 */
#define PC_FUF      02		/* Floating underflow */
#define PC_NODIV    01		/* No divide */
#if 0
word36 ir;			/* Instruction register */
#endif

#define zero(WORD) do {(WORD).hi = 0; (WORD).lo = 0;} while (0)
extern word36 zero36;
extern word36 one36;
extern int setting_pc;

#define page_to_word(PAGE) (PAGE << 9)

extern void (*opdisp[01000]) PARAMS ((int ac, addr10 ea));

/* Accumulators */
word36 acfile[8 * 16];		/* ac's (8 blocks worth) */
word36 *currentacblock;		/* The current AC block */
word36 *prevacblock;		/* Previous context AC block */
word36 *mem_acblock;		/* AC's when referenced as memory (used
				   for pxct 4). */
word36 *mem_acblock_1;		/* AC's when referenced as memory (used
				   for pxct 1). */
word36 *mem_acblock_2;		/* AC's when referenced as memory (used
				   for pxct 2). */
word36 *ea_acblock;		/* AC's when referenced during ea calc (used
				   for pxct). */

/* Interrupt definitions */
/* The following bits are unique for each 'device' that can generate interrupts
 */
#define APR_BIT 1
#define DTE_BIT 2
#define PI_BIT 4
#define TIMER_BIT 010
#define RH0_BIT 020
#define KNI_BIT 040

/* PI device */

#define PICLEARINT    0020000
#define PICLEARSYSTEM 0010000
#define PISETINT      0004000
#define PILEVON       0002000
#define PILEVOFF      0001000
#define PISYSOFF      0000400
#define PISYSON       0000200

int ppireq;			/* Program requested interrupts (really, the
				   upper half of the coni pi, */
int piconi;			/* PI coni/cono bits */

volatile int interrupt;		/* Next int level to service if >= 0, no int
				   pending if < 0 */

/* DTE device */

#define DTETO11DB     0020000	/* To-11 doorbell */
#define DTETO10DB     0001000	/* To-10 doorbell */
#define DTEPIENB      0000020	/* Set PI level */

int dteconi;			/* DTE bits */

/* MTR device */

int mtrconi;

#define BLOCKEDSIGS {SIGIO, SIGALRM, SIGVTALRM, SIGUSR1}
sigset_t blockedsigs;

/* Device control block */

#ifdef __STDC__
struct dcb;
#endif

typedef void io_func_type PARAMS ((struct dcb *dcb, int dev, int opcode, addr10 ea));

struct dcb
{
  int *hi_coni_ptr;
  int *lo_coni_ptr;
  int *hi_datai_ptr;
  int *lo_datai_ptr;

  io_func_type *(*funcs)[8];
};

/* Instruction prolog */

#if 0
#define INST(NAME, OPCODE) \
void CONCAT (i_,NAME) PARAMS ((int opcode, int ac, addr10 ea)); \
void CONCAT (i_,NAME)(opcode, ac, ea) \
       register int opcode, ac; \
       register addr10 ea;

#define IO_INST(OP, DEVNAME, DEVCODE) \
void CONCAT4 (i_,OP,_,DEVNAME) PARAMS ((int opcode, int ac, addr10 ea)); \
void CONCAT4 (i_,OP,_,DEVNAME)(opcode, ac, ea) \
       register int opcode, ac; \
       register addr10 ea;

#define SPECIAL_INST(NAME, OPCODE) \
void CONCAT (i_,NAME) PARAMS ((int opcode, int ac, addr10 ea)); \
void CONCAT (i_,NAME)(opcode, ac, ea) \
       register int opcode, ac; \
       register addr10 ea;

#define SPECIAL_IO_INST(OP, DEVNAME, DEVCODE) \
void CONCAT4 (i_,OP,_,DEVNAME) PARAMS ((int opcode, int ac, addr10 ea)); \
void CONCAT4 (i_,OP,_,DEVNAME)(opcode, ac, ea) \
       register int opcode, ac; \
       register addr10 ea;
#else
#define INST(NAME, OPCODE) \
void CONCAT (i_,NAME) PARAMS ((int ac, addr10 ea)); \
void CONCAT (i_,NAME)(ac, ea) \
       register int ac; \
       register addr10 ea;

#define IO_INST(OP, DEVNAME, DEVCODE) \
io_func_type CONCAT4 (i_,OP,_,DEVNAME); \
void CONCAT4 (i_,OP,_,DEVNAME)(dcb, dev, opcode, ea) \
       struct dcb *dcb; \
       int dev; \
       int opcode; \
       register addr10 ea;

#define SPECIAL_INST(NAME, OPCODE) \
void CONCAT (i_,NAME) PARAMS ((int opcode, int ac, addr10 ea)); \
void CONCAT (i_,NAME)(opcode, ac, ea) \
       register int opcode, ac; \
       register addr10 ea;

#define SPECIAL_IO_INST(OP, DEVNAME, DEVCODE) \
void CONCAT4 (i_,OP,_,DEVNAME) PARAMS ((addr10 ea)); \
void CONCAT4 (i_,OP,_,DEVNAME)(ea) \
       register addr10 ea;
#endif

/* PAG device */

int pagerconi;			/* Pager coni/cono bits (PAG device*/
#define PAGKL 040000		/* KL pager */
#define PAGENA 020000		/* Pager enabled */
#define PAGEBR 017777		/* Exec base register */

int pagerdataihigh;		/* Upper 18 bits of pager datai */
int pagerdatailow;		/* Lower 18 bits of pager datai */

#define PAGSELECTACBLOCKS 0400000
#define PAGSELECTPCS      0200000 /* Select previous context section */
#define PAGLOADUBA	  0100000 /* Load user base address */
#define PAGSAVEKEPT	  0040000 /* Save kept entries */
#define PAGNOACCOUNTS	  0400000 /* Don't update accounts */

/* Page fault bits (shifted right by 9 to fit in 32 bit word) */
#define PF_USER		0400000000
#define PF_HARD		0200000000
#define   PF_HARD_ERROR_MASK 0370000000
#define   PF_AR_PARITY_ERROR 0360000000
#define PF_ACCESSIBLE	0100000000
#define PF_MODIFIED	0040000000
#define PF_WRITEABLE	0020000000
#define PF_WRITEREF	0010000000
#define PF_PUBLIC	0004000000
#define PF_CACHEABLE	0002000000
#define PF_KEEP		0001000000
#define PF_SOFT		0000400000
#define PF_PAGE		0000017777

/* Special constants for the pager.  These live in AC block 6! */
#define CSTMASKAC (acfile[6 * 16 + 0]) /* CST Mask AC */
#define CSTDATAAC (acfile[6 * 16 + 1]) /* CST Data AC */
#define CSTAC (acfile[6 * 16 + 2]) /* Core Status Table AC */
#define SPTAC (acfile[6 * 16 + 3]) /* Special Page Table AC */

#define GLOBAL     010000000000
#define ADDRMASK    07777777777
#define KLADDRMASK  00037777777
#define SECTMASK    07777000000
#define KLPAGEMASK    037777000
#define HWORDMASK   00000777777

extern addr10 pc_cache_vm;

/* Sign extend WORD.hi to 32 bit long */

#if 0
#define SXHI(WORD) ((signed int) ((WORD.hi & 0400000) ? \
				  (WORD.hi | ~HWORDMASK) : WORD.hi))
#endif

/* Sign extend WORD.lo to 32 bit long (according to sign bit of WORD.hi */

#define SXLO(WORD) ((signed int) ((WORD.hi & 0400000) ? \
				  (WORD.lo | ~HWORDMASK) : WORD.lo))

/* Sign extend a halfword */

#define SXHW(HW) ((signed int) (((HW) ^ 0400000) - 0400000))
#define SXHI(WORD) (SXHW ((WORD).hi))

/* Increment ea, taking GLOBALity into account */
/* XXX got a pathological case here with ea 6-35 of all 1s (sigh) */
#define increa(EA) ((EA & GLOBAL) ? (EA + 1) : ((EA & SECTMASK) | (EA + 1)))

/* PC manipulation macros */

#define incrpc pc = (pc + 1) & HWORDMASK
#define setpc(ADDR) do {pc = ADDR & HWORDMASK;		\
	pcsection = ADDR & (~GLOBAL & SECTMASK & ADDRMASK); } while (0)

/* This macro will convert 5 chars into a word36.  The first byte contains
 * the MSB of the word, and the last byte contains the least significant
 * four bits, right justified, high four bits must be 0.  This is also known as
 * core-dump mode.
 */

#define string_to_word36(STRING, WORD) \
	do { \
	WORD.hi = (STRING)[0] << 10 | (STRING)[1] << 2 | (STRING)[2] >> 6; \
	WORD.lo = ((STRING)[2] << 12 | (STRING)[3] << 4 | (STRING)[4]) & HWORDMASK; } while (0)

/* Convert 9 bytes into 2 PDP-10 words.  This is known as high-density mode,
   and is used on disks and tapes.
 */

/* Macro's to convert word offsets to byte offsets depending upon the mode.  */

/* High-density is 4.5 bytes/word */
#define HD_WORD_TO_BYTE(w) (((w) * 9) / 2)

#define hd_to_word36(STRING, WORD0, WORD1) \
  do { \
    (WORD0).hi = (STRING)[0] << 10 | (STRING)[1] << 2 | (STRING)[2] >> 6; \
    (WORD0).lo = ((STRING)[2] << 12 | (STRING)[3] << 4 | (STRING)[4] >> 4) & HWORDMASK; \
    (WORD1).hi = ((STRING)[4] << 14 | (STRING)[5] << 6 | (STRING)[6] >> 2) & HWORDMASK; \
    (WORD1).lo = ((STRING)[6] << 16 | (STRING)[7] << 8 | (STRING)[8]) & HWORDMASK; \
  } while (0)

/* Convert 2 PDP-10 words into 9 bytes in high-density mode. */

#define word36_to_hd(WORD0, WORD1, STRING) \
  do { \
    (STRING)[0] = (WORD0).hi >> 10; \
    (STRING)[1] = (WORD0).hi >> 2; \
    (STRING)[2] = (WORD0).hi << 6 | (WORD0).lo >> 12; \
    (STRING)[3] = (WORD0).lo >> 4; \
    (STRING)[4] = (WORD0).lo << 4 | (WORD1).hi >> 14; \
    (STRING)[5] = (WORD1).hi >> 6; \
    (STRING)[6] = (WORD1).hi << 2 | (WORD1).lo >> 16; \
    (STRING)[7] = (WORD1).lo >> 8; \
    (STRING)[8] = (WORD1).lo; \
  } while (0)

/* Format 4 bytes into industry compatible mode.  */

#define ic_to_word36(STRING, WORD) \
	do { \
	WORD.hi = (STRING)[0] << 10 | (STRING)[1] << 2 | (STRING)[2] >> 6; \
	WORD.lo = ((STRING)[2] << 12 | (STRING)[3] << 4) & HWORDMASK; \
	} while (0)

/* Format 5 bytes into ansi-ascii mode. */

#define aa_to_word36(STRING, WORD) do { \
	WORD.hi = (((STRING)[0] & 0177) << 11) | (((STRING)[1] & 0177) << 4) | ((STRING)[2] & 0177) >> 3; \
	WORD.lo = (((STRING)[2] & 07) << 15)   | (((STRING)[3] & 0177) << 8) | ((STRING)[4] & 0177) << 1 | (STRING)[4] >> 7; \
	} while (0)

/* Load a field at POS, SIZE from WORD.  POS is the position of the rightmost
 * bit in the field.  Note that POS is in PDP-10 bit order.  Also note that the
 * SIZE can't be larger than 32, and that SIZE + (35 - POS) must be <= 36 (ie:
 * can't cross word boundary).
 */
#define ldb(POS, SIZE, WORD) (LDB(35-(POS), (SIZE), (WORD)))

/* All the work is actually done in this macro, which is somewhat simpler
 * because it is called with more appropriate arguments
 */

#define LDB(SHIFT, SIZE, WORD) \
  ((SIZE) == 18 && ((SHIFT) == 0 || (SHIFT) == 18)) ? \
    (((SHIFT) == 0) ? (WORD).lo : (WORD).hi) : \
  (((SHIFT) >= 18 ? \
    (WORD).hi >> ((SHIFT) - 18) : \
    (((SHIFT) + (SIZE)) <= 18 ? \
     (WORD).lo >> (SHIFT) : \
     (((WORD).hi << (18 - (SHIFT))) | ((WORD).lo >> (SHIFT))))) & \
   MASK(SIZE))

#ifndef NBBY
#define NBBY 8			/* Fucking Linux! */
#endif

/* MASK creates a right justified SIZE bit mask.
 * SLMASK creates a SIZE bit mask shifted left by SHIFT bits
 * SRMASK creates a SIZE bit mask shifted right by SHIFT bits
 */
#define NBITSLONG (sizeof(unsigned long) * NBBY)
#define MASK(SIZE) ((SIZE) > 0 ? (~(unsigned long)0 >> (NBITSLONG - (SIZE))) : 0)
#define SLMASK(SIZE, SHIFT) (MASK(SIZE) << (SHIFT))
#define CSL(SIZE, SHIFT) (~SLMASK(SIZE, (SHIFT)))
#define SRMASK(SIZE, SHIFT) (MASK(SIZE) >> (SHIFT))
#define CSR(SIZE, SHIFT) (~SRMASK(SIZE, (SHIFT)))

/* Alternate mask routine.  More efficient, but SIZE must be >= 0 and < NBITSLONG. */

#define ALTMASK(SIZE) (~(~(unsigned long)0 << (SIZE)))

/* mask36 masks out all but the low AMT bits of WORD.  AMT must be >= 0 and <= 36. */

#define mask36(AMT, WORD) \
  if ((AMT) >= 18) \
    (WORD).hi &= ALTMASK ((AMT) - 18); \
  else \
    { \
      (WORD).hi = 0; \
      (WORD).lo &= ALTMASK ((AMT)); \
    }

#define dpb(POS, SIZE, VAL, WORD) DPB(35-(POS), SIZE, ((VAL)&MASK(SIZE)), WORD)

#define DPB(SHIFT, SIZE, VAL, WORD) \
  if ((SHIFT) >= 18)		/* Field is in hi */		\
    (WORD).hi = (((WORD).hi & CSL(SIZE, (SHIFT) - 18)) | \
		 ((VAL) << ((SHIFT) - 18))) & HWORDMASK; \
  else if ((SHIFT) + (SIZE) <= 18) /* Field is in lo */		\
    (WORD).lo = (((WORD).lo & CSL(SIZE, SHIFT)) | ((VAL) << (SHIFT))) & \
       MASK(18); \
  else { /* Arrgh, hard case.  Byte is in both pieces */ \
	   (WORD).hi = (((WORD).hi & CSR(SIZE, 18 - (SHIFT))) | \
			((VAL) >> (18 - (SHIFT)))) & HWORDMASK; \
    (WORD).lo = (((WORD).lo & CSL(SIZE, SHIFT)) | ((VAL) << (SHIFT))) & \
      MASK(18); \
  }

/* test if a bit is on */

#define biton(BIT, WORD) (BITON(35-(BIT), (WORD)))

#define BITON(SHIFT, WORD) (SHIFT) >= 18 ? \
  WORD.hi & SLMASK(1, (SHIFT) - 18) : \
  WORD.lo & SLMASK(1, SHIFT)

/* Halfword manipulation macros */
#define upper18(WORD) ((WORD).hi)
#define lower18(WORD) ((WORD).lo)

/* These macros avoid the masking operations that would be done by the
   equivalent dpb(ldb) combination, and therefore save a little time.  They
   should only be used when you can guarantee that the upper bits (above 18) in
   hi and lo of a word36 are zero.
*/

#define hw_ll(DEST, SOURCE) (DEST).hi = (SOURCE).hi
#define hw_rl(DEST, SOURCE) (DEST).hi = (SOURCE).lo
#define hw_rr(DEST, SOURCE) (DEST).lo = (SOURCE).lo
#define hw_lr(DEST, SOURCE) (DEST).lo = (SOURCE).hi

/* Macro's for setting, clearing, and complementing the bits in the left or
   right half of a word.  */

#define hw_l_clear_bits(WORD, MASK) (WORD).hi &= ~(MASK)
#define hw_l_set_bits(WORD, MASK) (WORD).hi |= ((MASK) & HWORDMASK)
#define hw_l_complement_bits(WORD, MASK) (WORD).hi ^= ((MASK) & HWORDMASK)

#define hw_r_clear_bits(WORD, MASK) (WORD).lo &= ~(MASK)
#define hw_r_set_bits(WORD, MASK) (WORD).lo |= ((MASK) & HWORDMASK)
#define hw_r_complement_bits(WORD, MASK) (WORD).lo ^= ((MASK) & HWORDMASK)

/* Memory macros */

/* Physical store/fetch */
#define pstore(LOC, WORD) memarray[LOC] = (WORD)

#define pfetch(LOC, WORD) (WORD) = memarray[LOC]

/* Virtual store/fetch */
#if 1
#define vfetch(LOC, WORD) Vfetch(LOC, &WORD)
#define vstore(LOC, WORD) Vstore(LOC, &WORD)
#define vfetch_i(LOC, WORD) Vfetch_i(LOC, &WORD)
#define vfetch_ea(LOC, WORD) Vfetch_ea(LOC, &WORD)
#define vfetch_1(LOC, WORD) Vfetch_1(LOC, &WORD)
#define vstore_1(LOC, WORD) Vstore_1(LOC, &WORD)
#define vfetch_2(LOC, WORD) Vfetch_2(LOC, &WORD)
#define vstore_2(LOC, WORD) Vstore_2(LOC, &WORD)
#else
#define vfetch(LOC, WORD) if (is_acref(LOC)) (WORD) = mem_acref(LOC); else Vfetch(LOC, &WORD)
#define vstore(LOC, WORD) if (is_acref(LOC)) mem_acref(LOC) = (WORD); else Vstore(LOC, &WORD)
#define vfetch_i(LOC, WORD) if (is_acref(LOC)) (WORD) = mem_acref(LOC); else Vfetch_i(LOC, &WORD)
#define vfetch_ea(LOC, WORD) if (is_acref(LOC)) (WORD) = ea_acref(LOC); else Vfetch_ea(LOC, &WORD)
#define vfetch_1(LOC, WORD) if (is_acref(LOC)) (WORD) = mem_acref_1(LOC); else Vfetch_1(LOC, &WORD)
#define vstore_1(LOC, WORD) if (is_acref(LOC)) mem_acref_1(LOC) = (WORD); else Vstore_1(LOC, &WORD)
#define vfetch_2(LOC, WORD) if (is_acref(LOC)) (WORD) = mem_acref_2(LOC); else Vfetch_2(LOC, &WORD)
#define vstore_2(LOC, WORD) if (is_acref(LOC)) mem_acref_2(LOC) = (WORD); else Vstore_2 (LOC, &WORD)
#endif

/* Strange, but the following definitions actually slow things down! */
#ifdef undef
#define vfetch(LOC, WORD) if (is_acref(LOC)) (WORD) = acref(LOC); else pfetch(LOC, WORD)
#define vstore(LOC, WORD) if (is_acref(LOC)) acref(LOC) = (WORD); else pstore(LOC, WORD)
#endif

/* #define acref(LOC) ((LOC & (GLOBAL | 0777760)) == 0 || \
		    ((LOC & GLOBAL) && (LOC & 07776777760) == 0))
*/
/* Returns true if LOC references an AC */
#define is_acref(LOC) (((LOC) & 0777760) ? 0 /* In-section part not 0-17 */ \
		    : (			/* In sect part 0-17 */		\
		       !((LOC) & GLOBAL) ? 1 /* Local, must be AC */	\
		       : (		/* Global */			\
			  ((LOC) & 07776000000) ? 0 /* Not sect 0 or 1 */ \
			  : 1))) /* Sect 0 or 1 */

/* Returns something that can be used to reference an AC */
#define acref(LOC) (currentacblock[(LOC) & 017])

/* Read an AC when referenced as a memory loc */
#define mem_acref(LOC) (mem_acblock[(LOC) & 017])

/* Read an AC when referenced as memory for pxct 1 */
#define mem_acref_1(LOC) (mem_acblock_1[(LOC) & 017])

/* Read an AC when referenced as memory for pxct 2 */
#define mem_acref_2(LOC) (mem_acblock_2[(LOC) & 017])

/* Read an AC when referenced during an EA calc */
#define ea_acref(LOC) (ea_acblock[(LOC) & 017])

/* Define various handy ways to get at the ac for an instruction.  Note that
 * these macros currently require 'ac' to be setup correctly, but that may
 * change in the future.
 */
#define AC acref(ac)
#define ACplus1 acref(ac+1)
#define ACplus2 acref(ac+2)
#define ACplus3 acref(ac+3)
#define ACplus4 acref(ac+4)
#define ACplus5 acref(ac+5)

/* Do a shift, dealing with signs properly.  Positive AMT means left
 * shift, negative means right shift.  We need to do things this way
 * because brain dead C doesn't guarantee the results of negative
 * shift amounts */

#define XSHIFT(QUANT, AMT) (((AMT) > 0) ?		\
			    ((QUANT) << (AMT)) :	\
			    ((QUANT) >> -(AMT)))

/* Shift word left by the appropriate amount.  We also deal with negative
 * shifts properly (by doing a right shift.)
 */

#define shift(AMT, WORD)						\
  if ((AMT) > 0)		/* Left shift */			\
     LSHIFT(AMT, WORD)							\
  else				/* Right shift */			\
     RSHIFT(-(AMT), WORD)

/* Shift WORD right by AMT.  Use this when you will only be shifting right.
   This avoids the sign test in shift().  */

#define rshift36(AMT, WORD) RSHIFT (AMT, WORD)
#define lshift36(AMT, WORD) LSHIFT (AMT, WORD)

#define LSHIFT(AMT, WORD)						\
{									\
  if ((AMT) < 18)							\
    {									\
      WORD.hi = ((WORD.hi << (AMT)) | (WORD.lo >> (18 - (AMT)))) & HWORDMASK; \
      WORD.lo = (WORD.lo << (AMT)) & HWORDMASK;				\
    }									\
  else									\
    {									\
      WORD.hi = (WORD.lo << ((AMT) - 18)) & HWORDMASK;			\
      WORD.lo = 0;							\
    }									\
}

#define RSHIFT(AMT, WORD)						\
{									\
  if ((AMT) < 18)							\
    {									\
      WORD.lo = ((WORD.lo >> (AMT)) | (WORD.hi << (18 - (AMT)))) & HWORDMASK; \
      WORD.hi >>= (AMT);						\
    }									\
  else									\
    {									\
      WORD.lo = WORD.hi >> ((AMT) - 18);				\
      WORD.hi = 0;							\
    }									\
}

/* Arithmetic shift WORD by AMT.  AMT must be between -36 and 36 inclusive.
   Shift left if AMT is positive, right otherwise.
 */

#define ashift(AMT, WORD)						\
  if ((AMT) > 0)		/* Left shift */			\
    ALSHIFT(AMT, WORD)							\
  else				/* Right shift */			\
    ARSHIFT(-(AMT), WORD)

#define ALSHIFT(AMT, WORD)						\
{									\
  signed int top, overflow = 0;						\
  int savebit = (WORD).hi & 0400000;					\
  word36 mask, tmp;							\
									\
  top = ((WORD).hi ^ 0400000) - 0400000; /* sign extend */		\
									\
  /* Generate a mask that covers the sign and all bits being shifted out.  */ \
  mask.lo = HWORDMASK;							\
  mask.hi = HWORDMASK;							\
  LSHIFT (36 - (AMT) - 1, mask);					\
  tmp = mask;								\
									\
  and36 (tmp, tmp, (WORD));						\
  if (!eq36 (tmp) && !ceq (tmp, mask))					\
    overflow = 1;							\
									\
  if ((AMT) < 18)							\
    {									\
      top = (top << (AMT)) | ((WORD).lo >> (18 - (AMT)));		\
      (WORD).lo = ((WORD).lo << (AMT)) & HWORDMASK;			\
    }									\
  else if ((AMT) <= 35)							\
    {									\
      top = (WORD).lo << ((AMT) - 18);					\
      (WORD).lo = 0;							\
    }									\
  else									\
    {	/* Doing a left shift of more than 35 bits.  All that's left is the \
	   sign bit.  If (WORD) is negative, we have an overflow because of the \
	   zeros shifted in from the right.  */				\
      (WORD).lo = 0;							\
      top = 0;								\
									\
      if (top < 0)							\
	overflow = 1;							\
      else								\
	overflow = 0;							\
    }									\
									\
  (WORD).hi = (top & (HWORDMASK >> 1)) | savebit;			\
									\
  if (overflow)								\
    setflags (PC_OV | PC_TRAP1);					\
}

#define ARSHIFT(AMT, WORD)						\
{									\
  signed int top;							\
									\
  top = ((WORD).hi ^ 0400000) - 0400000; /* sign extend */		\
									\
  if ((AMT) < 18)							\
    {									\
      (WORD).lo = (((WORD).lo >> (AMT)) | (top << (18 - (AMT)))) & HWORDMASK; \
      (WORD).hi = (top >> (AMT)) & HWORDMASK;				\
    }									\
  else if ((AMT) <= 35)							\
    {									\
      (WORD).lo = (top >> ((AMT) - 18)) & HWORDMASK;			\
      (WORD).hi = (top >> 31) & HWORDMASK; /* Sign extend */		\
    }									\
  else									\
    { /* Doing a right shift of more than 35 bits.  Just extend the sign \
	 throughout the entire word.  */				\
      (WORD).hi = (WORD).lo = (top >> 31) & HWORDMASK;			\
    }									\
}

/* and/or/xor two words together, putting result into first arg.  The _swapped
   forms do the appropriate operation with the halves of one of the operands
   swapped.  These are primarily used for the TSxx instructions.  */

#define and36(DEST, S1, S2) {(DEST).hi = (S1).hi & (S2).hi; (DEST).lo = (S1).lo & (S2).lo;}
#define and36_imm(DEST, S1, IMM) {(DEST).hi = 0; (DEST).lo = (S1).lo & (IMM);}
#define ior36(DEST, S1, S2) {(DEST).hi = (S1).hi | (S2).hi; (DEST).lo = (S1).lo | (S2).lo;}
#define ior36_imm(DEST, S1, IMM) {(DEST).lo = (S1).lo | ((IMM) & HWORDMASK);}
#define ior36_swapped(DEST, S1, S2) {(DEST).hi = (S1).lo | (S2).hi; (DEST).lo = (S1).hi | (S2).lo;}
#define xor36(DEST, S1, S2) {(DEST).hi = (S1).hi ^ (S2).hi; (DEST).lo = (S1).lo ^ (S2).lo;}
#define xor36_imm(DEST, S1, IMM) {(DEST).lo = (S1).lo ^ ((IMM) & HWORDMASK);}
#define xor36_swapped(DEST, S1, S2) {(DEST).hi = (S1).lo ^ (S2).hi; (DEST).lo = (S1).hi ^ (S2).lo;}
#define eqv36(DEST, S1, S2) {(DEST).hi = ((S1).hi ^ (S2).hi) ^ HWORDMASK; (DEST).lo = ((S1).lo ^ (S2).lo) ^ HWORDMASK;}
#define eqv36_imm(DEST, S1, IMM) {(DEST).hi = (S1).hi ^ HWORDMASK; (DEST).lo = ~((S1).lo ^ (IMM)) & HWORDMASK;}
#define nor36(DEST, S1, S2) {(DEST).hi = ((S1).hi | (S2).hi) ^ HWORDMASK; (DEST).lo = ((S1).lo | (S2).lo) ^ HWORDMASK;}
#define nor36_imm(DEST, S1, IMM) {(DEST).hi = (S1).hi ^ HWORDMASK; (DEST).lo = ~((S1).lo | (IMM)) & HWORDMASK;}
#define nand36(DEST, S1, S2) {(DEST).hi = ((S1).hi & (S2).hi) ^ HWORDMASK; (DEST).lo = ((S1).lo & (S2).lo) ^ HWORDMASK;}
#define nand36_imm(DEST, S1, IMM) {(DEST).hi = HWORDMASK; (DEST).lo = ((S1).lo & (IMM)) ^ HWORDMASK;}

/* and the complement of S1 with S2, returning the result in DEST */

#define andc36(DEST, S1, S2) {(DEST).hi = ~(S1).hi & (S2).hi; (DEST).lo = ~(S1).lo & (S2).lo;}
#define andc36_imm(DEST, S1, IMM) {(DEST).hi = 0; (DEST).lo = (~(S1).lo & (IMM)) & HWORDMASK;}
#define and36_cimm(DEST, S1, IMM) {(DEST).hi = (S1).hi; (DEST).lo = ((S1).lo & ~(IMM)) & HWORDMASK;}
#define andc36_swapped(DEST, S1, S2) {(DEST).hi = ~(S1).lo & (S2).hi; (DEST).lo = ~(S1).hi & (S2).lo;}

/* or the complement of S1 with S2, returning the result in DEST */
#define orc36(DEST, S1, S2) {(DEST).hi = ((S1).hi ^ HWORDMASK) | (S2).hi; \
			       (DEST).lo = ((S1).lo ^ HWORDMASK) | (S2).lo;}
#define orc36_imm(DEST, S1, IMM) {(DEST).hi = ((S1).hi ^ HWORDMASK); \
				    (DEST).lo = (~(S1).lo | (IMM)) & HWORDMASK;}
#define or36_cimm(DEST, S1, IMM) {(DEST).hi = HWORDMASK; \
				   (DEST).lo = ((S1).lo | ~(IMM)) & HWORDMASK;}

#define not36(DEST, SOURCE) {(DEST).hi = (SOURCE).hi ^ HWORDMASK; \
			       (DEST).lo = (SOURCE).lo ^ HWORDMASK;}

/* add/subtract two words */

#define add36(DEST, SOURCE) {  (DEST).lo += (SOURCE).lo; \
			       (DEST).hi += (SOURCE).hi; \
			       (DEST).hi += ((DEST).lo >> 18); \
			       (DEST).lo &= HWORDMASK; \
			       (DEST).hi &= HWORDMASK;}

#define INTERNAL_ADD36_FLAGS(DEST_HI, DEST_LO, SOURCE_HI, SOURCE_LO) \
{ \
    int oflow, cry0; \
      oflow = ~((DEST_HI) ^ (SOURCE_HI)); \
    (DEST_LO) += (SOURCE_LO); \
    (DEST_HI) += (SOURCE_HI); \
    (DEST_HI) += ((DEST_LO) >> 18); \
    cry0 = (DEST_HI) > HWORDMASK ? 1 : 0; \
    (DEST_LO) &= HWORDMASK; \
    (DEST_HI) &= HWORDMASK; \
    oflow &= (DEST_HI) ^ (SOURCE_HI); \
    if (oflow & 0400000) \
      if (cry0) \
        pcflags |= PC_CRY0 | PC_OV | PC_TRAP1; \
      else \
        pcflags |= PC_CRY1 | PC_OV | PC_TRAP1; \
    else if (cry0) \
      pcflags |= PC_CRY0 | PC_CRY1; \
}

#define add36_flags(DEST, SOURCE) \
  INTERNAL_ADD36_FLAGS ((DEST).hi, (DEST).lo, (SOURCE).hi, (SOURCE).lo)

#define add36_imm_flags(DEST, SOURCE) \
  INTERNAL_ADD36_FLAGS ((DEST).hi, (DEST).lo, (0), (SOURCE))

#define incr36(WORD) {(WORD).lo++; \
			(WORD).hi += (WORD).lo >> 18; \
			  (WORD).lo &= HWORDMASK; \
			    (WORD).hi &= HWORDMASK; }

#define incr36_flags(WORD) \
  INTERNAL_ADD36_FLAGS ((WORD).hi, (WORD).lo, (0), (1))

#define sub36(DEST, SOURCE) {(DEST).lo -= (SOURCE).lo; \
			       (DEST).hi -= (SOURCE).hi; \
				 (DEST).hi += ((signed int)(DEST).lo >> 18); \
				   (DEST).lo &= HWORDMASK; \
				     (DEST).hi &= HWORDMASK;}

#define INTERNAL_SUB36_FLAGS(DIFFERENCE_HI, DIFFERENCE_LO, MINUEND_HI, MINUEND_LO, SUBTRAHEND_HI, SUBTRAHEND_LO) \
{ \
    int oflow, cry0; \
    oflow = (MINUEND_HI) ^ (SUBTRAHEND_HI); \
    (DIFFERENCE_LO) = (MINUEND_LO) - (SUBTRAHEND_LO); \
    (DIFFERENCE_HI) = (MINUEND_HI) - (SUBTRAHEND_HI); \
    (DIFFERENCE_HI) += ((signed int)(DIFFERENCE_LO) >> 18); \
    cry0 = (DIFFERENCE_HI) > HWORDMASK ? 1 : 0; \
    (DIFFERENCE_LO) &= HWORDMASK; \
    (DIFFERENCE_HI) &= HWORDMASK; \
    oflow &= ~((DIFFERENCE_HI) ^ (SUBTRAHEND_HI)); \
    if (oflow & 0400000) \
      if (cry0) \
        pcflags |= PC_CRY0 | PC_OV | PC_TRAP1; \
      else \
        pcflags |= PC_CRY1 | PC_OV | PC_TRAP1; \
    else if (cry0) \
      pcflags |= PC_CRY0 | PC_CRY1; \
}

#define sub36_flags(DEST, SOURCE) \
  INTERNAL_SUB36_FLAGS ((DEST).hi, (DEST).lo, (DEST).hi, (DEST).lo, (SOURCE).hi, (SOURCE).lo)

#define sub36_imm_flags(DEST, SOURCE) \
  INTERNAL_SUB36_FLAGS ((DEST).hi, (DEST).lo, (DEST).hi, (DEST).lo, (0), (SOURCE))

/* Do 2's complement of SOURCE and store it into DEST */

#define neg36(DEST, SOURCE) {(DEST).lo = -(SOURCE).lo; \
			       (DEST).hi = -(SOURCE).hi; \
				 (DEST).hi += ((signed int)(DEST).lo >> 18); \
				   (DEST).lo &= HWORDMASK; \
				     (DEST).hi &= HWORDMASK;}

#define neg36_flags(DEST, SOURCE) \
  INTERNAL_SUB36_FLAGS ((DEST).hi, (DEST).lo, (0), (0), (SOURCE).hi, (SOURCE).lo)

#define decr36(WORD) {(WORD).lo--; \
		       (WORD).hi += ((signed int)(WORD).lo >> 18); \
			 (WORD).lo &= HWORDMASK; \
			   (WORD).hi &= HWORDMASK; }

/* We have to do this by adding -1 in order to make cry0 and cry1 come out
   correct.  See the hardware manual section for sos/soj if you don't believe
   this.  */

#define decr36_flags(WORD) \
  INTERNAL_ADD36_FLAGS ((WORD).hi, (WORD).lo, (HWORDMASK), (HWORDMASK))

/* 70 bit arithmetic */

#define INTERNAL_ADD72_FLAGS(DSTHI_HI, DSTHI_LO, DSTLO_HI, DSTLO_LO, SRCHI_HI, SRCHI_LO, SRCLO_HI, SRCLO_LO)	\
{									\
  int oflow, cry0;							\
  oflow = ~((DSTHI_HI) ^ (SRCHI_HI));					\
  (DSTLO_LO) += (SRCLO_LO);						\
  (DSTLO_HI) = ((DSTLO_HI) & 0377777) + ((SRCLO_HI) & 0377777);		\
  (DSTHI_LO) += (SRCHI_LO);						\
  (DSTHI_HI) += (SRCHI_HI);						\
									\
  (DSTLO_HI) += ((DSTLO_LO) >> 18);					\
  (DSTHI_LO) += ((DSTLO_HI) >> 17);					\
  (DSTHI_HI) += ((DSTHI_LO) >> 18);					\
  cry0 = (DSTHI_HI) > HWORDMASK ? 1 : 0;				\
									\
  (DSTLO_LO) &= HWORDMASK;						\
  (DSTLO_HI) = ((DSTLO_HI) & 0377777) | ((DSTHI_HI) & 0400000);		\
  (DSTHI_LO) &= HWORDMASK;						\
  (DSTHI_HI) &= HWORDMASK;						\
  oflow &= (DSTHI_HI) ^ (SRCHI_HI);					\
  if (oflow & 0400000)							\
    if (pcflags & PC_USER)						\
      if (cry0)								\
	pcflags |= PC_CRY0 | PC_OV | PC_TRAP1;				\
      else								\
	pcflags |= PC_CRY1 | PC_OV | PC_TRAP1;				\
    else								\
      if (cry0)								\
	pcflags |= PC_CRY0 | PC_TRAP1;					\
      else								\
	pcflags |= PC_CRY1 | PC_TRAP1;					\
  else if (cry0)							\
    pcflags |= PC_CRY0 | PC_CRY1;					\
}

#define add72(DSTHI, DSTLO, SRCHI, SRCLO)				\
{									\
  ((DSTLO).lo) += ((SRCLO).lo);						\
  ((DSTLO).hi) = (((DSTLO).hi) & 0377777) + (((SRCLO).hi) & 0377777);	\
  (DSTHI).lo += (SRCHI).lo;						\
  ((DSTHI).hi) += ((SRCHI).hi);						\
									\
  ((DSTLO).hi) += (((DSTLO).lo) >> 18);					\
  (DSTHI).lo += (((DSTLO).hi) >> 17);					\
  ((DSTHI).hi) += ((DSTHI).lo >> 18);					\
									\
  ((DSTLO).lo) &= HWORDMASK;						\
  ((DSTLO).hi) = (((DSTLO).hi) & 0377777) | (((DSTHI).hi) & 0400000);	\
  (DSTHI).lo &= HWORDMASK;						\
  ((DSTHI).hi) &= HWORDMASK;						\
}

#define add72_flags(DSTHI, DSTLO, SRCHI, SRCLO)				\
  INTERNAL_ADD72_FLAGS ((DSTHI).hi, (DSTHI).lo, (DSTLO).hi, (DSTLO).lo, \
			(SRCHI).hi, (SRCHI).lo, (SRCLO).hi, (SRCLO).lo)

#define incr72_flags(WRDHI, WRDLO)					\
  INTERNAL_ADD72_FLAGS ((WRDHI).hi, (WRDHI).lo, (WRDLO).hi, (WRDLO).lo, \
			0, 0, 0, 1)

#define sub72(DSTHI, DSTLO, SRCHI, SRCLO)				\
{									\
  int oflow, cry0;							\
									\
  oflow = (DSTHI).hi ^ (SRCHI).hi;					\
  (DSTLO).lo -= (SRCLO).lo;						\
  (DSTLO).hi = ((DSTLO).hi & 0377777) - ((SRCLO).hi & 0377777);		\
  (DSTHI).lo -= (SRCHI).lo;						\
  (DSTHI).hi -= (SRCHI).hi;						\
									\
  (DSTLO).hi += ((signed int)(DSTLO).lo >> 18);				\
  (DSTHI).lo += ((signed int)(DSTLO).hi >> 17);				\
  (DSTHI).hi += ((signed int)(DSTHI).lo >> 18);				\
  cry0 = (DSTHI).hi > HWORDMASK ? 1 : 0;				\
									\
  oflow &= (DSTHI).hi ^ (SRCHI).hi;					\
  if (oflow & 0400000)							\
    if (pcflags & PC_USER)						\
      if (cry0)								\
	pcflags |= PC_CRY0 | PC_OV | PC_TRAP1;				\
      else								\
	pcflags |= PC_CRY1 | PC_OV | PC_TRAP1;				\
    else								\
      if (cry0)								\
	pcflags |= PC_CRY0 | PC_TRAP1;					\
      else								\
	pcflags |= PC_CRY1 | PC_TRAP1;					\
  else if (cry0)							\
    pcflags |= PC_CRY0 | PC_CRY1;					\
									\
  (DSTLO).lo &= HWORDMASK;						\
  (DSTLO).hi = ((DSTLO).hi & 0377777) | ((DSTHI).hi & 0400000);		\
  (DSTHI).lo &= HWORDMASK;						\
  (DSTHI).hi &= HWORDMASK;						\
}

#define neg72(DSTHI, DSTLO, SRCHI, SRCLO)				\
{									\
  (DSTLO).lo = -(SRCLO).lo;						\
  (DSTLO).hi = -((SRCLO).hi & 0377777);					\
  (DSTHI).lo = -(SRCHI).lo;						\
  (DSTHI).hi = -(SRCHI).hi;						\
									\
  (DSTLO).hi += ((signed int)(DSTLO).lo >> 18);				\
  (DSTHI).lo += ((signed int)(DSTLO).hi >> 17);				\
  (DSTHI).hi += ((signed int)(DSTHI).lo >> 18);				\
									\
  (DSTLO).lo &= HWORDMASK;						\
  (DSTLO).hi &= 0377777;						\
  (DSTHI).lo &= HWORDMASK;						\
  (DSTHI).hi &= HWORDMASK;						\
}

#define neg144(D0, D1, D2, D3, S0, S1, S2, S3)				\
{									\
  (D3).lo = -(S3).lo;							\
  (D3).hi = -((S3).hi & 0377777);					\
									\
  (D2).lo = -(S2).lo;							\
  (D2).hi = -((S2).hi & 0377777);					\
									\
  (D1).lo = -(S1).lo;							\
  (D1).hi = -((S1).hi & 0377777);					\
									\
  (D0).lo = -(S0).lo;							\
  (D0).hi = -(S0).hi;							\
									\
  (D3).hi += ((signed int)(D3).lo >> 18);				\
  (D2).lo += ((signed int)(D3).hi >> 17);				\
  (D2).hi += ((signed int)(D2).lo >> 18);				\
  (D1).lo += ((signed int)(D2).hi >> 17);				\
  (D1).hi += ((signed int)(D1).lo >> 18);				\
  (D0).lo += ((signed int)(D1).hi >> 17);				\
  (D0).hi += ((signed int)(D0).lo >> 18);				\
									\
  (D3).lo &= HWORDMASK;							\
  (D3).hi &= 0377777;							\
  (D2).lo &= HWORDMASK;							\
  (D2).hi &= 0377777;							\
  (D1).lo &= HWORDMASK;							\
  (D1).hi &= 0377777;							\
  (D0).lo &= HWORDMASK;							\
  (D0).hi &= HWORDMASK;							\
}

/* test a word, and return whether it's < > or == to 0 */

#define gt36(WORD) (!le36(WORD))			/* >  0 */
#define ge36(WORD) (((WORD).hi & 0400000) == 0)		/* >= 0 */
#define ne36(WORD) (!eq36(WORD))			/* != 0 */
#define eq36(WORD) ((WORD).hi == 0 && (WORD).lo == 0)	/* == 0 */
#define le36(WORD) (lt36(WORD) || eq36(WORD))		/* <= 0 */
#define lt36(WORD) ((WORD).hi & 0400000)		/* <  0 */

/* compare two words, and return whether they are < > or == to each other */

#define ceq(W1, W2) ((W1).hi == (W2).hi && (W1).lo == (W2).lo)
#define cne(W1, W2) ((W1).hi != (W2).hi || (W1).lo != (W2).lo)
#define cge(W1, W2) ((SXHI(W1) > SXHI(W2)) || \
		     ((W1).hi == (W2).hi && SXLO(W1) >= SXLO(W2)))
#define cgt(W1, W2) ((SXHI(W1) > SXHI(W2)) || \
		     ((W1).hi == (W2).hi && SXLO(W1) > SXLO(W2)))
#define cle(W1, W2) ((SXHI(W1) < SXHI(W2)) || \
		     ((W1).hi == (W2).hi && SXLO(W1) <= SXLO(W2)))
#define clt(W1, W2) ((SXHI(W1) < SXHI(W2)) || \
		     ((W1).hi == (W2).hi && SXLO(W1) < SXLO(W2)))
#define cltu(W1, W2) ((W1).hi < (W2).hi || \
		      ((W1).hi == (W2).hi && (W1).lo < (W2).lo))

/* Compare a word with an immediate value.  Mostly used by CAIxx instructions.
 * Basically, comparison is performed against <0,,IMM>.
 */

#define ceq_i(W1, IMM) ((W1).hi == 0 && (W1).lo == ((IMM) & HWORDMASK))
#define cle_i(W1, IMM) (biton (0, W1) || \
			((W1).hi == 0 && SXLO (W1) <= ((IMM) & HWORDMASK)))
#define clt_i(W1, IMM) (biton (0, W1) || \
			((W1).hi == 0 && SXLO (W1) < ((IMM) & HWORDMASK)))

/* Return true if both W1 and W2 have any common bits set, otherwise return
   false.  The swapped form does the test with the halves swapped.  This is
   primarily used by the TSxx instructions.  */

#define testbits36(W1, W2) ((W1.hi & W2.hi) || (W1.lo & W2.lo))

#define testbits36_swapped(W1, W2) ((W1.hi & W2.lo) || (W1.lo & W2.hi))

/* 36 bit arithmetic */

#define add36const(WORD, CONST) {(WORD).lo += (CONST); \
				   (WORD).hi += (signed int)(WORD).lo >> 18; \
				     (WORD).lo &= HWORDMASK; \
				       (WORD).hi &= HWORDMASK; }

/* effective address calculation */

#define ieacalc(IR, SECTION) (ldb(17, 5, (IR)) ? /* Get I and X */ \
			      Ieacalc((IR), (SECTION)) : /* complex case */ \
			      (SECTION) | ldb(35, 18, (IR)))

#define min(A, B) ((A) <= (B) ? (A) : (B))

void io_init PARAMS ((void));
void rh20_init PARAMS ((void));
void console_init PARAMS ((void));
void genunimp PARAMS ((void));
void jam_input PARAMS ((char *p));
addr10 readexe PARAMS ((char *exefilename));
void console_reset PARAMS ((void));
void suspend PARAMS ((void));

void Vfetch_i PARAMS ((addr10 loc, word36 *word));
word36 *vfetch_i_ref PARAMS ((addr10 loc));
void Vfetch_ea PARAMS ((addr10 loc, word36 *word));
void Vfetch PARAMS ((addr10 loc, word36 *word));
void Vfetch_1 PARAMS ((addr10 loc, word36 *word));
void Vfetch_2 PARAMS ((addr10 loc, word36 *word));
void Vstore PARAMS ((addr10 loc, word36 *word));
void Vstore_1 PARAMS ((addr10 loc, word36 *word));

void ufetch PARAMS ((int offset, word36 *word));
void ustore PARAMS ((int offset, word36 word));

void efetch PARAMS ((int offset, word36 *loc));
void estore PARAMS ((int offset, word36 loc));

word36 map PARAMS ((addr10 loc));
void pager_init PARAMS ((void));
void goexec PARAMS ((void));
void gouser PARAMS ((void));
void set_nxm PARAMS ((addr10 loc, unsigned long pageinfo));
void set_previous_eacalc PARAMS ((void));
void set_previous_context_func PARAMS ((int func));
void clear_previous_context PARAMS ((void));

void compare_strings PARAMS ((int opcode, int ac, addr10 ea));
void movst PARAMS ((int opcode, int ac, addr10 ea, word36 e0));

addr10 Ieacalc PARAMS ((word36 ir, addr10 section));
addr10 jrstfeacalc PARAMS ((word36 ir, addr10 section, int *flags));
addr10 efiw_eacalc PARAMS ((word36 efiw, addr10 section));

void handle_interrupt PARAMS ((void));
void finish_interrupt PARAMS ((void));
void calc_interrupt PARAMS ((void));
void set_interrupt PARAMS ((int level, unsigned long device_bit));
void clear_interrupt PARAMS ((int level, unsigned long device_bit));

void setflags PARAMS ((int newflags));

void clear_tlb PARAMS ((int clear_keep));
void clrpt PARAMS ((addr10 loc));
void dte_doorbell PARAMS ((void));

void print36 PARAMS ((word36 word));

void dte_interrupt PARAMS ((void));
void process_receive_packet PARAMS ((void));

void ppc PARAMS ((void));

void i_trace PARAMS ((int opcode, int ac, addr10 ea));

void output_version PARAMS ((void));

int get_packet PARAMS ((unsigned char **packetp));

int init_ether PARAMS ((void));

void send_packet PARAMS ((unsigned char *packet, int packetlen));

/* Config file reading interface */

typedef struct _config
{
  int len;			/* Total length of config string */
  char data[1];			/* The actual config file */
} * Config_t;

typedef char * Config_ptr;

Config_t cfg;			/* Global config pointer for kx10.config */

Config_t ConfigFileOpen PARAMS ((const char *filename));
char *ConfigGetString PARAMS ((const Config_t config, Config_ptr *pptr, const char *name));
struct ether_addr *ConfigGetEthernetAddress PARAMS ((const Config_t config, Config_ptr *pptr, const char *name));
char *ConfigGetHostAddress PARAMS ((const Config_t config, Config_ptr *pptr, const char *name));
