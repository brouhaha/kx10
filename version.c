/* Version output routines for kx10, the PDP-10 emulator.
   Copyright (C) 1994, 1995  Stu Grossman

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
output_version ()
{
  printf ("kx10 version 0.95, Copyright (C) 1994, 1995, 1996 Stu Grossman\n\
kx10 comes with ABSOLUTELY NO WARRANTY; for details look at COPYING.\n\
This is free software, and you are welcome to redistribute it\n\
under certain conditions; look at COPYING for details.\n");
}
