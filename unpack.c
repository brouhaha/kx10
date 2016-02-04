/* High density => core dump format converter program for kx10, the PDP-10 emulator.
   Copyright (C) 1994  Stu Grossman

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

/* Unpack 4.5 bytes/word to 5 bytes per word */

#include <stdio.h>

main()
{
  unsigned char ibuf[9];
  unsigned char obuf[10];

  while (1)
    {
      int i;

      i = fread (ibuf, 1, 9, stdin);
      if (i != 9 && i != 5)
	{
	  if (i != 0)
	    {
	      fprintf (stderr, "Not multiple of 9 or 5 bytes!\n");
	      exit(1);
	    }
	  exit(0);
	}
      obuf[0] = ibuf[0];
      obuf[1] = ibuf[1];
      obuf[2] = ibuf[2];
      obuf[3] = ibuf[3];
      obuf[4] = ibuf[4] >> 4;

      obuf[5] = (ibuf[4] & 017) << 4 | (ibuf[5] >> 4);
      obuf[6] = (ibuf[5] & 017) << 4 | (ibuf[6] >> 4);
      obuf[7] = (ibuf[6] & 017) << 4 | (ibuf[7] >> 4);
      obuf[8] = (ibuf[7] & 017) << 4 | (ibuf[8] >> 4);
      obuf[9] = ibuf[8] & 017;
      if (i == 9)
	fwrite (obuf, 1, 10, stdout);
      else
	fwrite (obuf, 1, 5, stdout);
    }
}
