/* Utility routines for kx10, the PDP-10 emulator.
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

void
print_instruction (ir)
     word36 ir;
{
  static char *iocodes[] = {"blki", "datai", "blko", "datao", "cono",
			      "coni", "consz", "conso"};
  static char *iodevs[] = {"apr", "pi", "pag", "cca", "tim", "mtr"};
  static char *extends[] = {"0", "cmpsl", "cmpse", "cmpsle", "edit", "cmpsge",
			      "cmpsn", "cmpsg",
			      "cvtdbo", "cvtdbt", "cvtbdo", "cvtbdt",
			      "movso", "movst", "movslj", "movsrj",
			      "xblt",
			      "gsngl", "gdble", "gdfix", "gfix", "gdfixr",
			      "gfixr", "dgfltr", "gfltr", "gfsc"};
  extern char *opnames[];
  int opcode, ac;
  int dev;

  opcode = ldb (8, 9, ir);
  ac = ldb (12, 4, ir);

  if (opcode >= 0700)
    {				/* I/O instruction */
      dev = ((opcode << 1) | (ac >> 3)) & 0177;

      printf("%s ", iocodes[ac & 7]);

      if (dev > 5)
	printf("%o", dev << 2);
      else
	printf("%s", iodevs[dev]);
    }
  else
    {				/* Normal instruction */
      printf("%s ", opnames[opcode]);
      if (ac)
	printf("%o,", ac);
    }

  if (ldb (13, 1, ir))		/* Deal with I and Y */
    putchar ('@');
  if (lower18 (ir) != 0)
    printf ("%o", lower18 (ir));

  if (ldb (17, 4, ir))		/* X (index) */
    printf ("(%lo)\r\n", ldb (17, 4, ir));
  else
    printf ("\r\n");
}

void
xneg36 (dst, src)
     word36 *dst;
     word36 src;
{
  if (upper18(src) == 0400000
      && lower18(src) == 0)
    printf ("neg36 trying to negate setz %o,,%o\r\n", (pcflags << 5)|(pcsection >> 18), pc);

  not36(*dst, src);
  incr36(*dst);
}

/* Print out a 72 bit quantity */

void
print72 (dword1, dword2)
     word36 dword1, dword2;
{
  printf ("%06o,%06o,%06o,%06o", upper18 (dword1), lower18 (dword1),
	  upper18 (dword2), lower18 (dword2));
}

/* Print out a 36 bit word as octal halfwords */

void
print36(word)
     word36 word;
{
  printf("%06o,,%06o", upper18(word), lower18(word));
}

void
pa(ac)
     int ac;
{
  print36(acref(ac));
}

#ifdef PROCESS_TRACING
int trace_ubr;
#endif

void
i_trace (opcode, ac, ea)
     int opcode;
     int ac;
     addr10 ea;
{
#ifdef PCHIST
  ppc ();
  i_uuo (opcode, ac, ea);
#else
# ifdef PROCESS_TRACING
  if (!trace_ubr)
    {
      trace_ubr = pagerdatailow;
      printf ("[kx10: instruction tracing ON]\r\n");
    }
  else
    {
      trace_ubr = 0;
      printf ("[kx10: Turning OFF instruction tracing]\r\n");
    }
  fflush (stdout);
# else /* PROCESS_TRACING */
  i_uuo (opcode, ac, ea);
# endif
#endif
}

/* ppc() - Print out PC history buffer, and dump stack also */

void
ppc ()
{
  int i;
  word36 mem;
  addr10 addr;
  extern addr10 pchist[];
  extern int pchp;
  int count;

  printf("\r\n");

  /* Dump accumulators */

  for (i=0; i<=15; i++) {
    pa(i);
    printf(" ");
  }
  printf("\r\n");

#ifdef PCHIST
  /* Dump PC history */

  for(i = 1; i <= 100; i++) printf("%06o ", pchist[(pchp - i) % 1024]);
  printf("\r\n");
#endif /* PCHIST */

#if 0
  /* Dump stack */

  count = upper18(acref(017));
  if (pcsection && count <= 0377777 && count != 0)
    addr = ldb(35, 30, acref(017)); /* Get 30 bit stack addr from ac */
  else
    addr = pcsection | ldb(35, 18, acref(017)); /* Local stack pointer */

  for(i = 1; i <= 100; i++)
    {
      vfetch(addr, mem);
      addr--;
      print36(mem);
      printf(" ");
    }
#endif

  printf("\r\n");
  fflush(stdout);
}

/* Print out a 36 bit memory location */

void
pl36(ea)
     addr10 ea;
{
  word36 tmp;

  vfetch(ea, tmp);

  printf("%06o,,%06o\n", upper18(tmp), lower18(tmp));
}
