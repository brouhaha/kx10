/* Config file reader for kx10, the PDP-10 emulator.
   Copyright (C) 1995 Stu Grossman

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
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <netdb.h>

/* Size of the constant part of Config_t */
#define configsize (sizeof (struct _config) - 1)

Config_t
ConfigFileOpen (filename)
     const char *filename;
{
  FILE *f;
  Config_t configinfo;
  long len;

  f = fopen (filename, "r");

  if (!f)
    return NULL;

  fseek (f, 0, 2);		/* Go to end of file */
  len = ftell (f);		/* Get the length of the file */
  fseek (f, 0, 0);		/* Back to the beginning */

  configinfo = malloc (configsize + len + 1);
  fread (configinfo->data, 1, len, f);
  configinfo->data[len] = '\000'; /* Tie it off with a NUL */
  configinfo->len = len;

  return configinfo;
}

/* Lookup NAME in CONFIG, starting at PPTR.  Returns a pointer to the start of
   the value associated with NAME.  The pointer at PPTR (if not NULL) is
   adjusted to point to the next line of the config file.  */

static char *
lookupval (configinfo, pptr, name)
     const Config_t configinfo;
     Config_ptr *pptr;
     const char *name;
{
  char *ptr;
  char *endptr;
  int namelen;
  char *val;

  if (!configinfo)
    return NULL;

  namelen = strlen (name);

  endptr = configinfo->data + configinfo->len;

  if (pptr)
    {
      if (*pptr >= endptr)
	return NULL;

      ptr = *pptr;
    }
  else
    ptr = configinfo->data;

  while (ptr < endptr)
    {
      /* We should now be at the start of a line */

      /* Skip leading whitespace */
      while (*ptr == ' ' || *ptr == '\t')
	ptr++;

      if ((ptr[namelen] == ' '
	   || ptr[namelen] == '\t')
	  && strncasecmp (name, ptr, namelen) == 0)
	{			/* Found name */
	  /* Skip whitespace between name and value */
	  ptr += namelen + 1;
	  while (*ptr == ' ' || *ptr == '\t')
	    ptr++;

	  val = ptr;

	  /* Find end of value */
	  while (*ptr != '\n'
		 && *ptr != '\000')
	    ptr++;
	  *ptr++ = '\000';	/* Tie off value */
	  if (pptr)
	    *pptr = ptr;
	  return val;
	}
      else
	{			/* Didn't find name */
	  /* Skip this line (which might end with a NUL) */
	  while (*ptr != '\n'
		 && *ptr != '\000')
	    ptr++;
	  ptr++;		/* Skip end of line char */
	}
    }
  return NULL;
}

char *
ConfigGetString (configinfo, pptr, name)
     const Config_t configinfo;
     Config_ptr *pptr;
     const char *name;
{
  char *p;

  p = lookupval (configinfo, pptr, name);

  return p;
}

struct ether_addr *
ConfigGetEthernetAddress (configinfo, pptr, name)
     const Config_t configinfo;
     Config_ptr *pptr;
     const char *name;
{
  char *p;
  extern struct ether_addr *ether_aton PARAMS ((char *s));

  p = lookupval (configinfo, pptr, name);

  if (!p)
    return NULL;

  return ether_aton (p);
}

char *
ConfigGetHostAddress (configinfo, pptr, name)
     const Config_t configinfo;
     Config_ptr *pptr;
     const char *name;
{
  char *p;
  struct hostent *he;

  p = lookupval (configinfo, pptr, name);

  if (!p)
    return NULL;

  he = gethostbyname (p);

  if (!he)
    return NULL;

  return he->h_addr_list[0];
}
