/* Dummy ethernet driver for kx10, the PDP-10 emulator.
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

#include "pdp10.h"

int
get_packet (packetp)
     unsigned char **packetp;
{
  return 0;
}

void
send_packet (packet, packetlen)
     unsigned char *packet;
     int packetlen;
{
}

int
init_ether ()
{
  return 0;
}
