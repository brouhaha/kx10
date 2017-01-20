/* Executable loading routines for kx10, the PDP-10 emulator.
   Copyright (C) 1991, 1992, 1993, 1994  Stu Grossman

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

/* Loadup executables into pseudo-core. */

#include "pdp10.h"
#include <string.h>

static addr10 readexb PARAMS ((void));

static addr10 readdmp PARAMS ((void));

FILE *exefd;

/* Read a 36 bit word from exefd */

static word36
get36()
{
  word36 thisword;
  unsigned char buf[5];

  if (fread(buf, 5, 1, exefd) != 1) {
    printf("Got an error from fread\n");
    exit(1);
  }

  string_to_word36(buf, thisword);

  return(thisword);
}

/* Read pages from the .EXE file into memory */

static void
load_pages(from, xto, count)
     register unsigned long from, xto;
     register unsigned long count;
{
  register word36 curword;

  count <<=  9;			/* Now it's a word count */
  from <<= 9;
  xto <<= 9;

  if (from) {			/* Non-zero page.  Copy contents */
    long curpos;

    curpos = ftell(exefd);
    fseek(exefd, from * 5, 0);

    while(count-- > 0) {
      curword = get36();
      pstore(xto, curword);
      xto++;
    }
    fseek(exefd, curpos, 0);
  } else {			/* Zero fill page */
    curword = zero36;
    while(count-- > 0) {
      pstore(xto, curword);
      xto++;
    }
  }

}

static int geta10num PARAMS ((int *termchar));

static int linenum;

static int
geta10num (termchar)
     int *termchar;
{
  int num;
  int c;
  int i;

  num = 0;

  for (i = 1; i <= 4; i++)
    {
      c = getc (exefd);
      switch (c)
	{
	case '\r':
	  continue;
	case '\n':
	  linenum++;
	case ',':
	case EOF:
	  if (num > 0177777)
	    {
	      fprintf (stderr, "geta10num:  Badly formatted number at line %d.\n", linenum);
	      exit (1);
	    }
	  *termchar = c;
	  return num;
	default:
	  num = (num << 6) | (c & 077);
	}
    }
  fprintf (stderr, "geta10num:  Number too long at line %d.\n", linenum);
  exit (1);
}

static int
geta10line (numbufp)
     int *numbufp;
{
  int num;
  int checksum;

  num = 0;
  checksum = 0;
  while (1)
    {
      int term;
      int x;

      x = geta10num (&term);
      checksum += x;
      *numbufp++ = x;
      num++;

      if (term != ',')
	break;
    }

  if ((checksum & 0177777) == 0)
    return num;

  fprintf (stderr, "Bad checksum at line %d.\n", linenum);
  exit (1);
}

/* Read an A10 file.  This is similar to a uuencoded file, but with checksums
   and a few more bells and whistles for zeroing and loading memory. */

static addr10
reada10 ()
{
  int linetype;
  int num;
  int numbuf[100];
  int *numbufp;
  int cnt;
  addr10 addr, lastaddr, firstaddr;

  firstaddr = -1;
  lastaddr = -1;
  linenum = 1;

  while (1)
    {
      int c;

      numbufp = numbuf;
      /* First char of line */
      c = getc (exefd);
      switch (c)
	{
	case ';':		/* Comment.  Ignore rest of line */
	  do
	    c = getc (exefd);
	  while (c != '\n' && c != EOF);
	  if (c == EOF)
	    break;
	  continue;
	case 'T':
	case 'Z':
	  linetype = c;

	  c = getc (exefd);

	  if (c != ' ')
	    fprintf (stderr, "Bad line format %c\n", c);

	  num = geta10line (numbuf);

	  cnt = numbuf[0] & 0377; /* # of 16 bit words */
	  addr = numbuf[1] | ((numbuf[0] << 2) & 0600000); /* -10 address */
	  numbufp = numbuf + 2;

	  if (linetype == 'Z')	/* Zero memory */
	    while (cnt-- > 0)
	      {
		int wrdcnt;

		wrdcnt = *numbufp++;
		fprintf (stderr, "[Zeroing memory from 0%o to 0%o]\n", addr,
			 addr + wrdcnt - 1);
		while (wrdcnt--)
		  {
		    pstore (addr, zero36);
		    addr++;
		  }
	      }
	  else			/* Load memory */
	    if (cnt == 0)	/* Start address */
	      return addr;
	    else
	      {
		if (addr != lastaddr)
		  {
		    if (lastaddr != -1)
		      if (lastaddr == firstaddr + 1)
			fprintf (stderr, "[Loading memory at 0%o]\n", firstaddr);
		      else
			fprintf (stderr, "[Loading memory from 0%o to 0%o]\n", firstaddr,
				 lastaddr - 1);
		    firstaddr = addr;
		  }

		for (; cnt > 0; cnt -= 3)
		  {
		    word36 mem;
		    int tmp;

		    tmp = *numbufp++;
		    dpb (35, 16, tmp, mem);

		    tmp = *numbufp++;
		    dpb (19, 16, tmp, mem);

		    tmp = *numbufp++;
		    dpb (3, 4, tmp, mem);

		    pstore (addr, mem);
		    addr++;
		  }
		lastaddr = addr;
	      }
	  continue;
	default:		/* Not a T, Z, or ; */
	  fprintf (stderr, "reada10:  Unknown line type `%c'.\n", c);
	  exit (1);
	}
      break;
    }
}

/* Read an .EXE file.  First part is the directory. This directory tells us where all the
 * bits go into memory.  Returns the address of the entry vector.
 */

addr10
readexe (exefilename)
     char *exefilename;
{
  word36 curword;
  int numwordsindir, blocktype;
  addr10 entryvector;
  extern int waits_flag;
  char *p;

  exefd = fopen (exefilename, "r");
  if (!exefd)
    {
      fprintf (stderr, "? File %s does not exist or is not readable\n",
	       exefilename);
      exit (1);
    }

  p = strrchr (exefilename, '.');

  if (p && strcmp (p, ".a10") == 0)
    return reada10 ();

  if (waits_flag)
    return readdmp ();

  curword = get36 ();
  if (upper18 (curword) != 01776)
    return readexb ();		/* Not an exe file, try exb file */

  printf ("File\tMem\tCount\tFlags\n");

  while (1)
    {
      blocktype = upper18 (curword);
      numwordsindir = lower18 (curword) - 1;

      switch (blocktype)
	{
	case 01776:		/* page map */
	  for (; numwordsindir > 0; numwordsindir -= 2)
	    {
	      unsigned filepage, procpage;
	      unsigned pagecount, flags;

	      curword = get36 ();
	      filepage = ldb (35, 27, curword);
	      flags = ldb (8, 9, curword);

	      curword = get36 ();
	      procpage = ldb (35, 27, curword);
	      pagecount = ldb (8, 9, curword) + 1;

	      printf ("%o\t%o\t%d\t%o\n", filepage, procpage, pagecount, flags);

	      load_pages (filepage, procpage, pagecount);
	    }
	  break;
	case 01775:		/* Entry vector location */
	  curword = get36 ();	/* Get entry vector length */
	  blocktype = ldb (35, 32, curword);
	  if (ldb (35, 32, curword) > 0)
	    {
	      curword = get36 ();
	      entryvector = ldb (35, 32, curword);
	      printf ("entryvector=%012o\n", entryvector);

	      return (entryvector);
	    }
	  break;
	case 01774:
	  while (numwordsindir--)
	    get36 ();
	  break;
	case 01777:
	  return (entryvector);
	default:
	  printf ("Illegal .EXE directory, bad blocktype\n");
	  exit (1);
	}
      curword = get36 ();
    }
}

/* Read an EXB (RSX boot image) file */

static int get8 PARAMS ((void));

static int
get8 ()
{
  static int pos = -1;
  static word36 curword;

  pos++;

  switch (pos & 3)
    {
    case 0:
      curword = get36 ();
      return ldb (17, 8, curword);
    case 1:
      return ldb (9, 8, curword);
    case 2:
      return ldb (35, 8, curword);
    case 3:
      return ldb (27, 8, curword);
    }
}

static addr10
readexb ()
{
  int odd = 0;

  fseek (exefd, 0, SEEK_SET);

  while (1)
    {
      int numbytes;
      addr10 addr;
      int biw = 0;
      int trash;

      if (odd)
	trash = get8 ();

      /* Count includes # of bytes of 36 bit word info + 4
	 for address. */

      numbytes = get8 ();
      numbytes |= get8 () << 8;
      numbytes -= 4;
      odd = numbytes & 1;

      addr = get8 ();
      addr |= get8 () << 8;
      addr |= get8 () << 16;
      addr |= get8 () << 24;

      if (numbytes == 0)
	return addr;		/* start address */

      for (; numbytes > 0; numbytes--, biw++)
	{
	  int curbyt;

	  curbyt = get8 ();

	  switch (biw)
	    {
	    case 0:
	      dpb (35, 8, curbyt, memarray[addr]);
	      break;
	    case 1:
	      dpb (27, 8, curbyt, memarray[addr]);
	      break;
	    case 2:
	      dpb (19, 8, curbyt, memarray[addr]);
	      break;
	    case 3:
	      dpb (11, 8, curbyt, memarray[addr]);
	      break;
	    case 4:
	      dpb (3, 4, curbyt, memarray[addr]);
	      addr++;
	      biw = -1;
	      break;
	    }
	}
    }
}

/* Read a Waits dmp file */

static addr10
readdmp ()
{
  unsigned char buf[5];
  addr10 addr = 074;
  word36 mem;
  extern int debug_monitor;

  while (1)
    {
      if (fread(buf, 5, 1, exefd) != 1)
	{
	  if (feof (exefd))
	    break;

	  printf("Got an error from fread\n");
	  exit(1);
	}
      string_to_word36(buf, memarray[addr]);
      addr++;
    }

  printf ("[Highest addr = 0%o]\r\n", addr - 1);

  pfetch (0116, mem);		/* Get .jbsym */
  pstore (036, mem);		/* Put it where eddt expects it */

  pstore (037, one36);		/* Fake out jobrel */

  if (!debug_monitor)
    pfetch (0120, mem);		/* Get jobsa */
  else
    pfetch (074, mem);		/* Get jobddt */

  addr = lower18 (mem);	/* Get start address */

  return addr;
}
