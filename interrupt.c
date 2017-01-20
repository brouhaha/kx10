/* Interrupt system emulation routines for kx10, the PDP-10 emulator.
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
#include <signal.h>

/*
Interrupts work as follows:
1) When a device wants to cause an interrupt, it calls set_interrupt() with
   a level number and a device-unique bit.  This bit is then set in the
   request word for the specified interrupt level.
2) The 'interrupt' variable has one bit for each possible interrupt level.  The
   level_mask variable is a ones complement mask of all the interrupt level
   bits for levels that are lower or equal to the priority of the current
   processor level.  Ie: if the processor is running at level 4, then the mask
   will have bits 7, 6, 5, and 4 cleared.  The disabled_mask variable is a
   ones complement mask of all levels which have been disabled via CONO PI,.
   So, the 'interrupt' variable is set by masking the indicated level number
   with level_mask and disabled_mask.

The clear_interrupt() routine works similarly, but it doesn't need to take
level_mask and disabled_mask into account, as it is simply clearing the
appropriate bits.

Using bit masks this way makes device emulation much easier, as the programmer
can call set/clear_interrupt() without trying to match the number of calls to
each.  This also closely resembles the hardware model.

Additionally, it will be pretty easy to recompute 'interrupt' whenever the
CONO PI is changed, or an XJEN, or interrupt occurs.
*/

static volatile int request[8] = {0}; /* Bit map of requests for each level */
#if 0
static int level_mask = ~0;	/* Mask for current interrupt level */
static int disabled_mask = ~0;	/* Mask of levels disabled by CONO PI, */
#endif

void
print_interrupts ()
{
  int i;

  if (interrupt != -1)
    {
      printf (", interrupt = %d", interrupt);

      for (i = 0; i <= 7; i++)
	if (request[i] != 0)
	  printf (", request[%d] = 0x%x", i, request[i]);
    }
}

void
calc_interrupt ()
{
  int level;

  if (request[0])		/* Level 0 is always on */
    interrupt = 0;
  else
    {
      if (piconi & PISYSON)
	for (level = 1; level <= 7; level++)
	  {
	    if (piconi & 0100000 >> level)
	      break;	/* Already holding an interrupt at this level */
	    if (piconi & 0200 >> level /* Level on? */
		&& request[level])
	      {
		interrupt = level;
		return;
	      }
	  }
      interrupt = -1;
    }
}

static void
print_sigprocmask ()
{
  sigset_t tmp;

  sigprocmask (SIG_BLOCK, NULL, &tmp);

  fprintf (stderr, "procmask = 0x%x\r\n", tmp);
  fflush (stderr);
}

/* Clear the highest interrupt level in preparation for doing a JEN/XJEN
   instruction.  */

void
finish_interrupt ()
{
  int level;
  sigset_t oldmask;

  sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

  for (level = 1; level <= 7; level++)
    if (piconi & 0100000 >> level)
      {
	piconi &= ~0100000 >> level; /* Make this level inactive */
	calc_interrupt();
	break;
      }
  sigprocmask (SIG_SETMASK, &oldmask, NULL);
}

void
handle_interrupt ()
{
  sigset_t oldmask;
  extern int rh0ivec;
  register int interrupt_tmp;

  interrupt_tmp = interrupt;

  if (interrupt_tmp == 0)
    {
      if (request[interrupt_tmp] & DTE_BIT)
	dte_interrupt ();
      else if (request[interrupt_tmp] & KNI_BIT)
	process_receive_packet ();
    }
  else if (interrupt_tmp > 0)	/* Normal vectored interrupt */
    {
      word36 ir;
      addr10 vector, ea;
      int opcode, ac;
      extern trace_ubr;

#ifdef PROCESS_TRACING
      if (pagerdatailow == trace_ubr
	  && pcflags & PC_USER)
	{
	  printf ("Interrupt prior to pc %o\r\n", pcsection | pc);
	  fflush (stdout);
	}
#endif

      if (request[interrupt_tmp] & DTE_BIT)
	vector = 0142;
      else if (request[interrupt_tmp] & TIMER_BIT)
	vector = 0514;
      else if (request[interrupt_tmp] & RH0_BIT)
	vector = rh0ivec;
      else
	vector = 040 + 2 * interrupt_tmp;

      sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);
      piconi |= 0100000 >> interrupt_tmp; /* Make this level active */
      calc_interrupt();
      sigprocmask (SIG_SETMASK, &oldmask, NULL);

    nextvec:
      efetch (vector, &ir);
      opcode = ldb (8, 9, ir);
      ac = ldb (12, 4, ir);
      if (pcflags & PC_USER)
	goexec ();
      ea = ieacalc (ir, 0);	/* Calc ea in section 0 */

      if (opcode == 0254 && ac == 07) /* xpcw? */
	{			/* Yes.  Use optimized inline code */
	  word36 flags, tmppc;

	  dpb (17, 18, pcflags << 5, flags);
	  dpb (35, 18, prev_pcsection >> 18, flags);
	  vstore (ea, flags);

	  dpb (5, 6, 0, tmppc);
	  dpb (35, 30, pcsection | pc, tmppc);
	  vstore (ea + 1, tmppc);

	  vfetch (ea + 2, flags);

	  vfetch (ea + 3, tmppc);

	  prev_pcsection = ldb (35, 5, flags) << 18;
	  pagerdataihigh = (pagerdataihigh & ~037) | (prev_pcsection >> 18);
	  pcflags = ldb (12, 13, flags);

	  setpc (ldb (35, 30, tmppc));
	}
      else if ((opcode >= 0700 && (ac & 015) == 0) /* blki/blko */
	       || ((opcode & 0700) == 0600) /* txxx */
	       || ((opcode & 0710) == 0310)) /* camx, skipx, aosx, sosx (should add caix) */
	{
	  addr10 oldpc;

	  /* Handle (most) skip instructions.  If instruction skips, we dismiss
	     the interrupt.  If not, we execute the instruction in the vector
	     location.  */

	  oldpc = pc;
	  (*opdisp[opcode]) (ac, ea); /* Recursively call the interpreter */

	  if (oldpc != pc)
	    {			/* It skipped.  Dismiss the interrupt. */
	      pc = oldpc;

	      if (pcflags & PC_USER)
		gouser ();
	      finish_interrupt ();
	      return;
	    }

	  /* Didn't skip.  Take instruction from next loc. */

	  vector++;
	  goto nextvec;
	}
      else if (opcode == 0265)	/* jsp */
	{
	  dpb (12, 13, pcflags, AC);
	  dpb (35, 23, pcsection|pc, AC);

	  if (pcflags & PC_USER)
	    pcflags &= PC_USER | PC_PUBLIC;

	  setpc (ea);
	}
      else if (opcode == 0264)	/* jsr */
	{
	  word36 tmp;

	  tmp = zero36;

	  if (pcsection)
	    {
	      dpb (35, 30, (pcsection | pc), tmp) /* 30 bit pc for non-zero sections */
	    }
	  else
	    {
	      dpb (12, 13, pcflags, tmp); /* PC and flags for sect 0 */
	      dpb (35, 18, pc, tmp);
	    }

	  vstore (ea, tmp);	/* Store current PC into (ea) */

	  if (pcflags & PC_USER)
	    pcflags &= PC_USER | PC_PUBLIC;

	  setpc (increa(ea));	/* Jump to ea+1 */
	}
      else if (opcode == 0260	/* pushj */
	       || opcode == 0266) /* jsa  */
	{
	  printf ("\r\nBad I/O instruction %o at vector %o, pc %o\r\n", opcode, vector, pc);
	  genunimp ();
	}
      else
	{
	  printf ("\r\nBad I/O instruction %o at vector %o, pc %o\r\n", opcode, vector, pc);
	  genunimp ();		/* All others undefined */
	}

      return;
    }
  else
    {
      printf ("Got to handle_interrupt() without interrupt being set\r\n");
      genunimp();
    }
}

/* Call this routine to indicate that an interrupt is desired for the
 * specified device at the
 * specified level.  When the interrupting condition has been satisfied,
 * clear_interrupt() must be called explicitly, as the simulated
 * interrupt system will not clear interrupts by itself.  Failure to do
 * this will result in a hung system, which is constantly servicing
 * interrupts at the orphaned level.
 */

void
set_interrupt(level, device_bit)
     int level;
     unsigned long device_bit;
{
  sigset_t oldmask;

  sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

  request[level & 07] |= device_bit;
  calc_interrupt();

  sigprocmask (SIG_SETMASK, &oldmask, NULL);
}

/* Drop an interrupt request.  This routine clears the device request for
 * the specified level.  Additionally, it will clear out the level request
 * if that level is now free.
 */

void
clear_interrupt(level, device_bit)
     int level;
     unsigned long device_bit;
{
  sigset_t oldmask;

  sigprocmask (SIG_BLOCK, &blockedsigs, &oldmask);

  request[level & 07] &= ~device_bit;
  calc_interrupt();

  sigprocmask (SIG_SETMASK, &oldmask, NULL);
}
