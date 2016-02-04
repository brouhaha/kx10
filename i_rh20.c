/* RH20 channel, disk and tape instructions for kx10, the PDP-10 emulator.
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

/* RH20 channel and disk/tape emulation */

#include "pdp10.h"
#include <sys/time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

struct channel;
struct disk_unit;

union punit
{
  struct generic_unit *g;
  struct disk_unit *disk;
  struct tape_unit *tape;
};

struct disk_params
{
  int type;			/* Drive type */
  int sec_per_cyl;		/* Sectors per cylinder */
  int sec_per_track;		/* Sectors per track */
  int ncyl;			/* Number of cylinders */
  void (*regaccess) PARAMS ((struct channel *channel,
			     union punit punit,
			     int regnum,
			     int *reg)); /* Call this for register access */
  int packed;			/* Whether disk is in core-dump or memory format */
};

struct disk_unit
{
  void (*regaccess) PARAMS ((struct channel *channel,
			     union punit punit,
			     int regnum,
			     int *reg)); /* Call this for register access */
  int dcreg;			/* Drive control reg */
  int dsreg;			/* Drive status reg */
  int tracksectreg;		/* Current track/sector */
  int laval;			/* Look ahead reg */
  int serial;			/* Drive serial # */
  int offset;			/* Cyl offset */
  int cyl;			/* Current cylinder number */
  int asreg;			/* Our bit in the attention summary reg */

  struct disk_params params;	/* Copy of drive parameter table */

  int fd;			/* FD of disk file */
  off_t lastfilepos;		/* Next disk pos for read/write */
};

struct tape_unit;

struct tape_params
{
  int type;			/* Drive type */
  void (*regaccess) PARAMS ((struct channel *channel,
			     union punit punit,
			     int regnum,
			     int *reg)); /* Call this for register access */
};

struct tape_unit
{
  void (*regaccess) PARAMS ((struct channel *channel,
			     union punit punit,
			     int regnum,
			     int *reg)); /* Call this for register access */
  int dcreg;			/* Drive control reg */
  int dsreg;			/* Drive status reg */
  short framecount;		/* # of frames to read/write */
  int serial;			/* Drive serial # */
  int control;			/* Tape control */
  int asreg;			/* Our bit in the attention summary reg */

  struct tape_params params;	/* Copy of drive parameter table */

  int fd;			/* FD of tape file */
  int doing_rmt;		/* Non-zero if doing rmt tape protocol */
};

struct generic_unit
{
  void (*regaccess) PARAMS ((struct channel *channel,
			     union punit punit,
			     int regnum,
			     int *reg)); /* Call this for register access */
};

int rh0ivec;			/* XXX - hack, cough, cough */

struct channel
{
  int coni;
  int pbar;			/* Primary block address register */
  int ptcr;			/* Primary xfer control reg */
  int sbar;			/* Secondary block address register */
  int stcr;			/* Secondary xfer control reg */

  int ivec;			/* Interrupt vector (EPT rel) */
  int intbit;			/* Internal interrupt device bit */
  int dataireg;
  int datairegnum;

  int asreg;			/* Unit attention summary reg */

  int logout;			/* Base of channel logout area */

  union punit unit[8];		/* Pointer to units/kontrollers */
};

static struct channel *channels[8];

#define REGWRITE 04000000000	/* Modify register */
#define REGXFER 0200000000	/* Successful transfer */

static void
init_channel (channel_num)
     int channel_num;
{
  struct channel *channel;

  channel = (struct channel *) calloc (1, sizeof (struct channel));

  if (!channel)
    exit (1);

  channel->coni = 0;
  channel->intbit = RH0_BIT << channel_num;
  channel->logout = channel_num * 4;

  channels[channel_num] = channel;
}

static void
init_disk (channel, unit_num, filename, params, disk_serial)
     struct channel *channel;
     int unit_num;
     char *filename;
     struct disk_params *params;
     int disk_serial;
{
  struct disk_unit *unit;

  if (!channel)
    abort ();

  unit = (struct disk_unit *) malloc (sizeof (struct disk_unit));
  unit->dsreg = 0;
  unit->lastfilepos = 0;

  if (!unit)
    abort ();

  unit->fd = open (filename, O_RDWR);

  if (unit->fd < 0)
    {
      int disksize;
      off_t fileoffset;

      switch (errno)
	{
	case ENOENT:		/* File doesn't exist.  Create it. */
	  disksize = params->sec_per_cyl * params->ncyl * 128;

	  unit->fd = open (filename, O_RDWR|O_CREAT, 0666);
	  if (unit->fd < 0)
	    {
	      perror ("Couldn't create disk file");
	      exit (1);
	    }
	  /* We write the last byte in the file to prevent read errors from random
	     access reads of a holey file.  */
	  if (params->packed)
	    fileoffset = HD_WORD_TO_BYTE (disksize);
	  else
	    fileoffset = disksize * sizeof (word36);

	  lseek (unit->fd, fileoffset - 1, SEEK_SET);

	  if (write (unit->fd, "", 1) != 1) /* Write the last byte in the file */
	    abort ();
	  unit->lastfilepos = disksize;
	  break;
	case EACCES:		/* Write protected? */
	  fprintf (stderr, "[kx10: file %s is write-protected.]\r\n", filename);
	  unit->fd = open (filename, O_RDONLY);	/* Try readonly */
	  unit->dsreg |= 04000;	/* Set write locked */
	  break;
	default:
	  break;
	}
    }

  if (unit->fd < 0)
    {
      perror ("Couldn't open disk file");
      exit (1);
    }

  unit->params = *params;
  unit->regaccess = params->regaccess;
  unit->serial = disk_serial;
  unit->dcreg = 04200;		/* This port avail & ready */
  unit->dsreg |= 010700;	/* Media online, drive present,
				   drive ready, volume valid */
  unit->asreg = 1 << unit_num;	/* Set attention summary bit */
  channel->unit[unit_num].disk = unit;
}

static int
init_tape (channel, unit_num, filename, params, tape_serial)
     struct channel *channel;
     int unit_num;
     char *filename;
     struct tape_params *params;
     int tape_serial;
{
  struct tape_unit *unit;
  char *p;

  if (!channel)
    abort ();

  unit = (struct tape_unit *) malloc (sizeof (struct tape_unit));

  if (!unit)
    abort ();

  if (strchr (filename, ':'))
    {
      char *p;

      

    }
  else
    {
      unit->fd = open (filename, O_RDONLY);
      unit->doing_rmt = 0;
    }

  if (unit->fd < 0)
    return 0;

  unit->params = *params;
  unit->params.type |= 042000;	/* Tape drive, slave present */
  unit->regaccess = params->regaccess;
  unit->serial = tape_serial;
  unit->dcreg = 04000;		/* Drive avail */
  unit->dsreg = 014602;		/* Media online, write lock,
				   drive present, drive ready, bot */
  unit->control = 0;		/* Set slave zero */
  unit->asreg = 1 << unit_num;	/* Set attention summary bit */
  channel->unit[unit_num].tape = unit;

  return 1;
}

static void disk_regaccess ();

static struct disk_params rp06_params = {
  022,				/* rp06 */
  19 * 20,			/* Sectors per cylinder */
  20,				/* Sectors per track */
  800,				/* Number of cylinders */
  disk_regaccess,		/* Register access routine */
  0,				/* Not packed */
};

static struct disk_params rp06_params_packed = {
  022,				/* rp06 */
  19 * 20,			/* Sectors per cylinder */
  20,				/* Sectors per track */
  800,				/* Number of cylinders */
  disk_regaccess,		/* Register access routine */
  0,				/* Not packed */
};

static struct disk_params rp07_params_packed = {
  042,				/* RP07 (ty.r7f=042, fixed head) (or ty.r7m=041, moving head) */
  32 * 43,			/* sec/cyl (tracks/cyl * sec/track) */
  43,				/* sec/track */
  629,				/* #cyl */
  disk_regaccess,		/* reg access */
  1,				/* packed */
};

static void tape_regaccess ();

static struct tape_params tu45_params = {
  014,				/* tu45 on a tm02 */
  tape_regaccess,		/* Register access routine */
};

int logfd;

static void init_kni ();

static struct disk_params * guess_disk_params PARAMS ((char *fname));

static struct disk_params *
guess_disk_params (fname)
     char *fname;
{
  struct stat sb;

  if (stat (fname, &sb) == 0)
    {
      unsigned long nwords;

      nwords = rp07_params_packed.sec_per_cyl * rp07_params_packed.ncyl * 128;

      if (sb.st_size == (nwords * 9) / 2)
	{
	  fprintf(stderr,"[kx10: %s is an RP07 in packed format]\r\n",fname);
	  return &rp07_params_packed;
	}

      nwords = rp06_params.sec_per_cyl * rp06_params.ncyl * 128;

      if ((sb.st_size == ((nwords * 9) / 2)) || (sb.st_size == 175102976)) /* Where does this number come from? */
	{
	  fprintf(stderr,"[kx10: %s is an RP06 in packed format]\r\n",fname);
	  return &rp06_params_packed;
	}

      if (sb.st_size == (nwords * 8))
	{
	  fprintf(stderr,"[kx10: %s is an RP06 in word36 format]\r\n",fname);
	  return &rp06_params;
	}

      fprintf(stderr,"[kx10: %s unknown disk type]\r\n",fname);
      return NULL;
    }
  else
    return NULL;
}

void
rh20_init ()
{
  extern int waits_flag;
  int i;
  int found_disk;

  init_channel (0);

  found_disk = 0;
  for (i = 0; i <= 7; i++)
    {
      struct disk_params *rp;
      char fname[20];

      sprintf (fname, "DISK-0-%o", i);

      rp = guess_disk_params (fname);

      if (!rp)
	continue;

      init_disk (channels[0], i, fname, rp, 1);
      found_disk = 1;
    }

  if (!found_disk)
    {
      fprintf (stderr, "[kx10: No disk files found.  Creating rp06 volume DISK-0-0.]\r\n");
      init_disk (channels[0], 0, "DISK-0-0", &rp06_params, 1);
    }

  logfd = open ("disklog", O_RDWR|O_CREAT, 0666);
  if (!init_tape (channels[0], 1, "TAPE-0-1", &tu45_params, 1))
    init_tape (channels[0], 1, "foo:/dev/nrmt0", &tu45_params, 1);

  if (!waits_flag)
    init_kni ();
}


static void
cono_rh20 (channel, ea)		/* rh20 cono */
     struct channel *channel;
     addr10 ea;
{
  if (!channel)
    return;			/* Ignore non-ex channels */

  if (ea & 0770000)
    {
      printf ("Unimplemented control bits in rh0: 0%o, pc=0%o\n", ea, pc);
      genunimp ();
    }

  if (ea & 04000)		/* Clear RAE */
    channel->coni &= ~04000;

  if (ea & 02000)		/* Controller reset */
    channel->coni = 0;

  if (ea & 01000)		/* Clear transfer error */
    channel->coni &= ~01000;		/* Actually, this is data overrun */

  if (ea & 0200)		/* Reset command list */
    ;				/* XXX - do nothing for now */

  if (ea & 0100)		/* Delete secondary command */
    channel->coni &= ~0100;

  if (ea & 020)			/* Stop primary command */
    channel->coni &= ~020;

  if (ea & 010)			/* Clear done */
    {
      channel->coni &= ~010;
      if (channel->coni & 07)
	clear_interrupt (channel->coni, RH0_BIT);
    }

  /* Simply copy Massbus enable, Attention enable & pi level */

  channel->coni = (channel->coni & ~0447) | (ea & 0447);
}

static void
coni_rh20 (channel, ea)
     struct channel *channel;
     addr10 ea;
{
  word36 tmp;

  tmp = zero36;

  if (channel)
    dpb (35, 18, channel->coni, tmp);

  vstore(ea, tmp);
}

#ifdef DISK_PROFILE
static long long readtimes = 0;
static long readcount = 0;

static inline int
myread (int fd, void *buf, int size)
{
  struct timeval now, then;
  int retval;
  long t;

  gettimeofday (&now, NULL);
  retval = read (fd, buf, size);
  gettimeofday (&then, NULL);

  t = then.tv_usec - now.tv_usec;
  if (t < 0)
    t += 1000000;

  readtimes += t;
  readcount++;

  return retval;
}

static long long writetimes = 0;
static long writecount = 0;

static inline int
mywrite (int fd, void *buf, int size)
{
  struct timeval now, then;
  int retval;
  long t;

  gettimeofday (&now, NULL);
  retval = write (fd, buf, size);
  gettimeofday (&then, NULL);

  t = then.tv_usec - now.tv_usec;
  if (t < 0)
    t += 1000000;

  writetimes += t;
  writecount++;

  return retval;
}

void
print_io_times ()
{
  printf ("Read count = %d, read time %lld\r\n", readcount, readtimes);
  printf ("Write count = %d, write time %lld\r\n", writecount, writetimes);
}

#endif /* DISK_PROFILE */

/* Come here when drive control register is written.  This actually initiates
   I/O.  */

static void
disk_func (channel, unit)
     struct channel *channel;
     struct disk_unit *unit;
{
  switch (unit->dcreg & 077)
    {
    case 07:			/* recal -  XXX check with H/W ref */
    case 023:			/* pack ack */
    case 021:			/* readin preset */
      return;
    case 011:			/* Drive clear */
      unit->dsreg = 010700;
      unit->tracksectreg = 0;
      unit->cyl = 0;
      return;
    case 5:			/* Seek */
      unit->dsreg |= 0100000;	/* Attention */
      channel->asreg |= unit->asreg; /* Set attention summary */
      channel->coni |= 0200;	/* Set attention bit in coni */
      if (channel->coni & 040	/* Channel interrupt enable? */
	  && channel->coni & 07) /* PIA set? */
	set_interrupt (channel->coni, channel->intbit);
      return;			/* Just a no-op */
    case 061:			/* Write */
    case 071:			/* Read */
      {
	word36 icw;
	off_t filepos;
	unsigned long physaddr;
	int fcn;
	int blkcnt;
	int wordcnt;
	addr10 addr;
	int track;
	int sector;

	track = (unit->tracksectreg >> 8) & 037;
	sector = unit->tracksectreg & 0377;

	filepos = unit->cyl * unit->params.sec_per_cyl + track * unit->params.sec_per_track
	  + sector;

	filepos *= 128;

	efetch (channel->logout + 0, &icw); /* Fetch initial icw from ept */

	wordcnt = 0;
	physaddr = 0;
	while (1)
	  {
	    fcn = ldb (2, 3, icw); /* Get func code */

	    if (fcn == 0)
	      break;		/* halt */

	    if (fcn == 2)	/* Jump */
	      addr = ldb (35, 22, icw);
	    else if (fcn == 6)
	      {			/* fwd data xfer (halt) */
		wordcnt += ldb (13, 11, icw);
		physaddr += ldb (35, 22, icw);
		break;
	      }
	    else if (fcn == 4)
	      {			/* fwd data xfer */
		int tmp;

		tmp = ldb (35, 22, icw);
		if (tmp != 0)
		  {
		    printf ("non-halt CCW must be a skip!\r\n");
		    genunimp ();
		  }
		
		tmp = ldb (13, 11, icw); /* Get count */
		wordcnt -= tmp;	/* Skip words */
		filepos += tmp;
		addr++;		/* Next channel word */
	      }
	    else
	      {
		printf ("CCW is bad, func = %o\r\n", fcn);
		genunimp ();
	      }

	    pfetch (addr, icw);
	  }

	if (filepos != unit->lastfilepos)
	  {
	    off_t fileoffset;

	    if (unit->params.packed)
	      fileoffset = HD_WORD_TO_BYTE (filepos);
	    else
	      fileoffset = filepos * sizeof (word36);

	    if (lseek (unit->fd, fileoffset, SEEK_SET) == -1)
	      {
		printf ("lseek error! errno = %d\r\n", errno);
		genunimp ();
		return;
	      }
	    unit->lastfilepos = filepos;
	  }

	blkcnt = (channel->ptcr >> 6) & 01777;
	blkcnt = (~blkcnt + 1) & 01777;

#if 0
	if (wordcnt != blkcnt * 128)
	  {
	    printf ("Disagreement between wordcnt and blkcnt\r\n");
	    genunimp ();
	  }
#endif

	if ((unit->dcreg & 077) == 061)
	  {
	    if (unit->params.packed)
	      {
		unsigned char *p, *buf;
		word36 *wp;

		if ((filepos & 1) || (wordcnt & 1))
		  {
		    printf ("filepos (0x%x) or wordcnt (%d) not even.\r\n", filepos, wordcnt);
		    genunimp ();
		    return;
		  }

		buf = alloca (HD_WORD_TO_BYTE (wordcnt));

		p = buf;
		for (wp = memarray + physaddr;
		     wp < memarray + physaddr + wordcnt;
		     wp += 2, p += 9)
		  word36_to_hd (wp[0], wp[1], p);

		if (mywrite (unit->fd, buf, HD_WORD_TO_BYTE (wordcnt))
		    != HD_WORD_TO_BYTE (wordcnt))
		  {
		    printf ("Disk write failure, errno = %d\r\n", errno);
		    genunimp ();
		    return;
		  }
	      }
	    else
	      if (mywrite (unit->fd, &memarray[physaddr], wordcnt * sizeof (word36))
		  != wordcnt * sizeof (word36))
		{
		  printf ("Disk write failure, errno = %d\r\n", errno);
		  genunimp ();
		  return;
		}
	  }
	else
	  {
	    if (unit->params.packed)
	      {
		unsigned char *p, *buf;
		word36 *wp;

		if ((filepos & 1) || (wordcnt & 1))
		  {
		    printf ("filepos (0x%x) or wordcnt (%d) not even.\r\n", filepos, wordcnt);
		    genunimp ();
		    return;
		  }

		buf = alloca (HD_WORD_TO_BYTE (wordcnt));

		if (myread (unit->fd, buf, HD_WORD_TO_BYTE (wordcnt))
		    != HD_WORD_TO_BYTE (wordcnt))
		  {
		    printf ("Disk read failure, errno = %d\r\n", errno);
		    genunimp ();
		    return;
		  }

		p = buf;
		for (wp = memarray + physaddr;
		     wp < memarray + physaddr + wordcnt;
		     wp += 2, p += 9)
		  hd_to_word36 (p, wp[0], wp[1]);
	      }
	    else
	      if (myread (unit->fd, &memarray[physaddr], wordcnt * sizeof (word36))
		  != wordcnt * sizeof (word36))
		{
		  printf ("Disk read failure, errno = %d\r\n", errno);
		  genunimp ();
		}
	  }

	unit->lastfilepos += wordcnt;

	/* Now, setup channel logout words */

	dpb (13, 14, 050000 >> 1, icw); /* Good status */
	dpb (35, 22, addr + 1, icw); /* addr + 1 of last cmd word */
	estore (channel->logout+1, icw);

	dpb (2, 3, fcn, icw);	/* Current func */
	dpb (13, 11, 0, icw);	/* Remaining word count */
	dpb (35, 22, physaddr + wordcnt, icw); /* Addr of last wrd xfered */
	estore (channel->logout+2, icw);
	channel->coni |= 010;		/* Xfer done */
	if (channel->coni & 040	/* Channel interrupt enable? */
	    && channel->coni & 07)	/* PIA set? */
	  set_interrupt (channel->coni, channel->intbit);

      }
      return;
    }

  printf ("Unknown drive function 0%o\r\n", unit->dcreg);
}

static void
disk_regaccess (channel, punit, regnum, reg)
     struct channel *channel;
     union punit punit;
     int regnum;
     int *reg;
{
  struct disk_unit *unit = punit.disk;

  switch (regnum)
    {
    case 00:			/* Drive control reg */
      if (*reg & REGWRITE)
	{
	  unit->dcreg = (unit->dcreg & ~043577) | (*reg & 043577);
	  disk_func (channel, unit); /* Execute the specified function */
	}
      else
	*reg = unit->dcreg;
      return;
    case 01:			/* Drive status register */
      if (*reg & REGWRITE)
	return;			/* Can't write this reg */
      *reg = unit->dsreg;
      return;
    case 04:			/* Attention summary register */
      if (*reg & REGWRITE)
	{
	  channel->asreg &= ~(*reg & 0377); /* Turn off specified attention bits */
	  unit->dsreg &= ~0100000; /* Turn off drive attention too */
	  channel->coni &= ~0200; /* Turn it off in coni as well */
	  if (channel->coni & 040 /* Channel interrupt enabled? */
	      && channel->coni & 07) /* PIA set? */
	    clear_interrupt (channel->coni, RH0_BIT);
	}
      else
	*reg = channel->asreg;
      return;
    case 05:			/* Track/sector register */
      if (*reg & REGWRITE)
	unit->tracksectreg = *reg;
      else
	*reg = unit->tracksectreg;
      return;
    case 06:			/* Drive type register*/
      if (*reg & REGWRITE)
	return;			/* Can't write this reg! */
      *reg = unit->params.type;
      return;
    case 07:			/* Look ahead (sector #) */
      if (*reg & REGWRITE)
	{
	  printf ("Trying to write drive reg 0%o\r\n", regnum);
	  genunimp();
	  return;
	}
      *reg = unit->laval++;
      /* Randomize look-ahead val to fool monitor */
      unit->laval = (unit->laval + 0100) & 03760;
      return;
    case 010:			/* Drive serial number register */
      if (*reg & REGWRITE)
	{
	  printf ("Trying to write drive reg 0%o\r\n", regnum);
	  genunimp();
	  return;
	}
      *reg = unit->serial;
      return;
    case 011:			/* Offset register */
      if (*reg & REGWRITE)
	unit->offset = *reg;
      else
	*reg = unit->offset;
      return;
    case 012:			/* Cylinder register */
      if (*reg & REGWRITE)
	unit->cyl = *reg & 0177777;
      else
	*reg = unit->cyl;
      return;
    default:
      printf ("Unknown bits for rh0 datao: 0%o\n", *reg);
      genunimp ();
    }
}

/* Come here when drive control register is written.  This actually initiates
   I/O.  */
#if 0
#include <sys/mtio.h>

static int rmt_buflen = 0;
static char *rmt_bufp;
static char rmt_buf[100];

static int
rmt_getc (unit)
     struct tape_unit *unit;
{
  if (rmt_buflen-- > 0)
    return *rmt_bufp++;

  do
    rmt_buflen = read (unit->fd, rmt_buf, sizeof rmt_buf);
  while (rmt_buflen == 0);

  if (rmt_buflen < 0)
    return -1;

  rmt_bufp = rmt_buf;

  return rmt_getc (unit);
}

static int
rmt_read (unit, buf, len)
     struct tape_unit *unit;
     unsigned char *buf;
     size_t len;
{
  int tmp;

  tmp = min (len, rmt_buflen);
  memcpy (buf, rmt_buf, tmp);
  rmt_buflen = 0;

  if (tmp >= len)
    return len;

  for (buf += tmp, len -= tmp;
       len > 0;
       len -= tmp)
    {
      tmp = read (unit->fd, buf, len);

      if (tmp < 0)
	return len;
    }

  return len;
}

static int
rmt_send_cmd (unit, cmd)
     struct tape_unit *unit;
     char *cmd;
{
  char resp[100], *p;
  int cc;
  int resp_code;
  int retval;

  cc = write (unit->fd, cmd, strlen (cmd));
  if (cc != strlen (cmd))
    {
      if (cc >= 0)
	fprintf (stderr, "rmt protocol error: command write too short: %d %lu\r\n",
		 cc, strlen (cmd));
      return -1;
    }

  resp_code = rmt_getc (unit);
  if (resp_code == -1)
    {
      fprintf (stderr, "rmt protocol error: couldn't get response code\r\n");
      return -1;
    }

  for (p = resp; p < resp + sizeof resp - 1; p++)
    {
      int c;

      c = rmt_getc (unit);
      if (c == -1)
	{
	  fprintf (stderr, "rmt protocol error: couldn't get response number\r\n");
	  return -1;
	}

      *p = c;

      if (c == '\n')
	break;
    }

  *p = '\000';

  if (p[-1] != '\n')
    {
      fprintf (stderr, "rmt protocol error: Unterminated response number: %s\r\n", resp);
      return -1;
    }

  retval = strtol (resp, &p, 10);
  if (!p || *p != '\n')
    {
      
      fprintf (stderr, "rmt protocol error: couldn't parse response number: %s\r\n", resp);
      return -1;
    }

  switch (resp_code)
    {
    case 'A':			/* Normal case */
      return retval;
    case 'E':			/* Error code */
      errno = retval;		/* *crock* */
      while (1)
	{
	  int c;

	  c = rmt_getc (unit);
	  if (c == -1 || c == '\n')
	    break;
	}
      return -1;
    }

  fprintf (stderr, "rmt protocol error: Unknown response: %c%s\r\n", resp_code, resp);
  return -1;
}

static int
tape_read (unit, buf, len)
     struct tape_unit *unit;
     unsigned char *buf;
     size_t len;
{
  if (unit->doing_rmt)
    {
      int cc;

      sprintf (buf, "R%lu\n", len);
      cc = rmt_send_cmd (unit, buf);
      if (cc < 0)
	return -1;
      return rmt_read (unit, buf, cc);
    }
  else
    return read (unit->fd, buf, len);
}
#endif

static void
tape_func (channel, unit)
     struct channel *channel;
     struct tape_unit *unit;
{
#if 0
  struct mtop mtop;
#endif

  switch (unit->dcreg & 077)
    {
    case 03:			/* Rewind and unload */
    case 07:			/* Rewind */
      lseek (unit->fd, 0, SEEK_SET);
#if 0
      mtop.mt_op = MTREW;
      ioctl (unit->fd, MTIOCTOP, &mtop); /* In case it's a real tape drive */
#endif
#if 1 /* hack alert */
      close (unit->fd);
      unit->fd = open ("TAPE-0-1", O_RDONLY);
#endif
      unit->dsreg |= 0100002;	/* Set attention and BOT */
      unit->dsreg &= ~04;	/* Clear tape mark */
      channel->asreg |= unit->asreg; /* Set attention summary */
      channel->coni |= 0200;	/* Set attention bit in coni */
      if (channel->coni & 040	/* Channel interrupt enable? */
	  && channel->coni & 07) /* PIA set? */
	set_interrupt (channel->coni, channel->intbit);
      return;
    case 011:			/* Drive clear */
      return;
    case 031:			/* Space fwd by records */
#if 0
      mtop.mt_op = MTFSR;
      mtop.mt_count = unit->framecount;
#endif
      lseek (unit->fd, -unit->framecount * 518 * 5, SEEK_CUR);
#if 0
      ioctl (unit->fd, MTIOCTOP, &mtop);
#endif
      /* We actually just pretend we hit a tape mark */
      unit->framecount = 0;
      unit->dsreg |= 0100000;	/* Set attention */
      channel->asreg |= unit->asreg; /* Set attention summary */
      channel->coni |= 0200;	/* Set attention bit in coni */
      if (channel->coni & 040	/* Channel interrupt enable? */
	  && channel->coni & 07) /* PIA set? */
	set_interrupt (channel->coni, channel->intbit);
      return;
    case 033:			/* Space backwards by records */
#if 0
      mtop.mt_op = MTBSR;
      mtop.mt_count = unit->framecount;
#endif
      lseek (unit->fd, unit->framecount * 518 * 5, SEEK_CUR);
#if 0
      ioctl (unit->fd, MTIOCTOP, &mtop);
#endif
      unit->framecount = 0;	/* Pretend we skipped all records */
      unit->dsreg |= 0100000;	/* Set attention */
      channel->asreg |= unit->asreg; /* Set attention summary */
      channel->coni |= 0200;	/* Set attention bit in coni */
      if (channel->coni & 040	/* Channel interrupt enable? */
	  && channel->coni & 07) /* PIA set? */
	set_interrupt (channel->coni, channel->intbit);
      return;
    case 071:			/* Read */
      {
	word36 icw;
	unsigned long physaddr;
	int fcn;
	int blkcnt;
	int wordcnt;
	int framecount;
	int i;
	addr10 addr;
	unsigned char *framebuf;

	unit->framecount = 0;

	efetch (channel->logout+0, &icw); /* Fetch initial icw from ept */

	while (1)		/* Loop over all ccws */
	  {
	    fcn = ldb (2, 3, icw); /* Get func code */

	    switch (fcn)
	      {
	      case 2:		/* Jump */
		addr = ldb (35, 22, icw);
		pfetch (addr, icw);
		continue;
	      case 4:		/* fwd data xfer */
	      case 6:		/* fwd data xfer (halt) */
		wordcnt = ldb (13, 11, icw);
		physaddr = ldb (35, 22, icw);
		break;
	      default:
		printf ("Initial CCW is bad\r\n");
		genunimp ();
		break;
	      }
#if 0
	    printf ("[0%o: fcn = 0%o, %d words to 0%o]\r\n", addr, fcn,
		    wordcnt, physaddr);
#endif

	    blkcnt = (channel->ptcr >> 6) & 01777;
	    blkcnt = (~blkcnt + 1) & 01777;

	    switch (unit->control & 0360)
	      {
	      case 00:		/* Core dump mode */
	      case 040:		/* Ansi-ascii mode */
		framecount = wordcnt * 5;
		break;
	      case 060:		/* Industry compatible mode */
		framecount = wordcnt * 4;
		break;
	      default:
		printf ("Trying to use unsupported tape mode 0%o\r\n", (unit->control >> 4) & 017);
	      }

	    framecount = min (framecount, 518 * 5 - unit->framecount);

#if 0
	    printf ("framecount = %d\r\n", framecount);
#endif

	    framebuf = (unsigned char *) alloca (framecount);

#if 0
	    framecount = tape_read (unit, framebuf, framecount);
#else
	    framecount = read (unit->fd, framebuf, framecount);
#endif
	    if (framecount < 0)
	      {
		printf ("Tape read failure, errno = %d\r\n", errno);
		genunimp ();
	      }

	    unit->dsreg &= ~042; /* Clear bot & tape mark */

	    if (framecount == 0)
	      unit->dsreg |= 04; /* Set tape mark */

	    unit->framecount += framecount;

	    /* Now, reformat the core dump format data into word36 format */

	    switch (unit->control & 0360)
	      {
	      case 00:		/* Core dump mode */
		for (i = 0; i < framecount; i += 5)
		  {
		    string_to_word36 (&framebuf[i], memarray[physaddr]);
		    physaddr++;
		    wordcnt--;
		  }
#if 0
		if (i - framecount > 0)
		  printf ("i - framecount is %d\r\n", i - framecount);
#endif
		switch (i - framecount)
		  {
		  case 1:
		    dpb (35, 4, 0, memarray[physaddr - 1]);
		    break;
		  case 2:
		    dpb (35, 12, 0, memarray[physaddr - 1]);
		    break;
		  case 3:
		    dpb (35, 20, 0, memarray[physaddr - 1]);
		    break;
		  case 4:
		    dpb (35, 28, 0, memarray[physaddr - 1]);
		    break;
		  }
		break;
	      case 040:		/* Ansi-ascii mode */
		for (i = 0; i < framecount; i += 5)
		  {
		    aa_to_word36 (&framebuf[i], memarray[physaddr]);
		    physaddr++;
		    wordcnt--;
		  }
		switch (i - framecount)
		  {
		  case 1:
		    dpb (35, 8, 0, memarray[physaddr - 1]);
		    break;
		  case 2:
		    dpb (35, 15, 0, memarray[physaddr - 1]);
		    break;
		  case 3:
		    dpb (35, 22, 0, memarray[physaddr - 1]);
		    break;
		  case 4:
		    dpb (35, 29, 0, memarray[physaddr - 1]);
		    break;
		  }
		break;
	      case 060:		/* Industry compatible mode */
		for (i = 0; i < framecount; i += 4)
		  {
		    ic_to_word36 (&framebuf[i], memarray[physaddr]);
		    physaddr++;
		    wordcnt--;
		  }
		switch (i - framecount)
		  {
		  case 1:
		    dpb (35, 12, 0, memarray[physaddr - 1]);
		    break;
		  case 2:
		    dpb (35, 20, 0, memarray[physaddr - 1]);
		    break;
		  case 3:
		    dpb (35, 28, 0, memarray[physaddr - 1]);
		    break;
		  }
		break;
	      }

	    if (unit->framecount >= 518 * 5)
	      break;

	    if (fcn & 2)
	      break;		/* It's a halt ccw, we're done */
	    addr++;
	    pfetch (addr, icw);
	  }

#if 0
	printf ("unit->framecount = %d\r\n", unit->framecount);
#endif

	/* Now, setup channel logout words */

	dpb (13, 14, 050000 >> 1, icw); /* Good status */
	if (wordcnt > 0)
	  {
	    dpb (11, 1, 1, icw); /* Long word count (record too short) */
	  }
	dpb (35, 22, addr + 1, icw); /* addr + 1 of last cmd word */
	estore (channel->logout+1, icw);

	dpb (2, 3, fcn, icw);	/* Current func */
	dpb (13, 11, wordcnt, icw); /* Remaining word count */
	dpb (35, 22, physaddr - 1, icw); /* Addr of last wrd xfered */
	estore (channel->logout+2, icw);
	channel->coni |= 010;		/* Xfer done */
	if (channel->coni & 040	/* Channel interrupt enable? */
	    && channel->coni & 07)	/* PIA set? */
	  set_interrupt (channel->coni, channel->intbit);
      }
      return;
    }

  printf ("Unknown tape drive function 0%o\r\n", unit->dcreg);
}

static void
tape_regaccess (channel, punit, regnum, reg)
     struct channel *channel;
     union punit punit;
     int regnum;
     int *reg;
{
  struct tape_unit *unit = punit.tape;

  switch (regnum)
    {
    case 00:			/* Drive control reg */
      if (*reg & REGWRITE)
	{
	  unit->dcreg = *reg | 04000; /* Drive avail */
	  tape_func (channel, unit); /* Execute the specified function */
	}
      else
	*reg = unit->dcreg;
      return;
    case 01:			/* Drive status register */
      if (*reg & REGWRITE)
	return;			/* Can't write this reg */
      *reg = unit->dsreg;
      return;
    case 02:			/* Error register */
      if (*reg & REGWRITE)
	return;			/* Can't write this reg */
      *reg = 0;			/* Pretend no errors */
      return;
    case 04:			/* Attention summary register */
      if (*reg & REGWRITE)
	{
	  channel->asreg &= ~(*reg & 0377); /* Turn off specified attention bits */
	  unit->dsreg &= ~0100000; /* Turn off drive attention too */
	  channel->coni &= ~0200; /* Turn it off in coni as well */
	  if (channel->coni & 040 /* Channel interrupt enabled? */
	      && channel->coni & 07) /* PIA set? */
	    clear_interrupt (channel->coni, RH0_BIT);
	}
      else
	*reg = channel->asreg;
      return;
    case 05:			/* Frame count reg */
      if (*reg & REGWRITE)
	unit->framecount = *reg;
      else
	*reg = unit->framecount;
      return;
    case 06:			/* Drive type register*/
      if (*reg & REGWRITE)
	return;			/* Can't write this reg! */
      *reg = unit->params.type;
      return;
    case 010:			/* Drive serial number register */
      if (*reg & REGWRITE)
	{
	  printf ("Trying to write drive reg 0%o\r\n", regnum);
	  genunimp();
	  return;
	}
      *reg = unit->serial;
      return;
    case 011:			/* Tape control reg */
      if (*reg & REGWRITE)
	{
	  unit->control = *reg;

	  /* Set slave present only for slave 0 */
	  if ((unit->control & 07) != 0)
	    unit->params.type &= ~02000;
	  else
	    unit->params.type |= 02000;
	}
      else
	*reg = unit->control;
      return;
    default:
      printf ("Unknown bits for rh0 datao: 0%o\n", *reg);
      genunimp ();
    }
}

static void
datao_rh20 (channel, ea)
     struct channel *channel;
     addr10 ea;
{
  union punit punit;

  word36 mem;
  int reg;
  int regnum;

  if (!channel)
    return;

  vfetch(ea, mem);

  regnum = ldb (5, 6, mem);
  reg = ldb (35, 30, mem);
  punit = channel->unit[ldb (17, 3, mem)];

  channel->datairegnum = regnum;
  channel->dataireg = reg;

  switch (regnum)
    {
    case 070:			/* Secondary block address */
      if (reg & REGWRITE)
	{
	  channel->pbar = reg;
	  punit.g->regaccess (channel, punit, 5, &reg);
	}
      else
	channel->dataireg = channel->pbar;
      return;
    case 071:			/* Secondary xfer control reg */
      if (reg & REGWRITE)
	{
	  channel->ptcr = reg;
	  punit.g->regaccess (channel, punit, 0, &reg);
#if 0
	  channel->coni |= 020;	/* Primary cmd full */
#endif
	}
      else
	channel->dataireg = channel->ptcr;
      return;
    case 072:			/* Primary block address */
      if (reg & REGWRITE)
	{
	  printf ("Trying to write pbar!\r\n");
	  genunimp ();
	}
      else
	channel->dataireg = channel->pbar;
      return;
    case 073:			/* Primary xfer control reg */
      if (reg & REGWRITE)
	{
	  printf ("Trying to write ptcr!\r\n");
	  genunimp ();
	}
      else
	channel->dataireg = channel->ptcr;
      return;
    case 074:			/* Interrupt vector address */
      if (reg & REGWRITE)
	{
	  channel->ivec = reg;
	  rh0ivec = reg & 0777;	/* XXX - hack, cough, cough */
	}
      else
	channel->dataireg = channel->ivec;
      return;
    case 075:
    case 076:
    case 077:
      goto badrhbits;

    default:
      break;
    }

  /* It's a drive register.  Call the unit driver. */

  if ((reg & ~04407777777) != 0)
    goto badrhbits;

  reg &= ~0400000000;		/* Ignore disable RAE stop */

  if (!punit.g)
    {
      channel->dataireg = 0;	/* Just zero for now */
      return;
    }

  punit.g->regaccess (channel, punit, regnum, &reg);

  channel->dataireg = reg | REGXFER;

  return;

 badrhbits:
  printf ("Unknown bits for rh0 datao: ");
  print36 (mem);
  printf ("\n");
  genunimp ();
}

static void
datai_rh20 (channel, ea)
     struct channel *channel;
     addr10 ea;
{
  word36 mem;

  if (!channel)
    {
      vstore(ea, zero36);
      return;
    }

  dpb (5, 6, channel->datairegnum, mem);
  dpb (35, 30, channel->dataireg, mem);

  vstore(ea, mem);
}

extern io_func_type i_unimp_unknown;

static io_func_type i_datai_rh0;
static io_func_type i_datao_rh0;
static io_func_type i_cono_rh0;
static io_func_type i_coni_rh0;
static io_func_type i_consz_rh0;
static io_func_type i_conso_rh0;

static io_func_type *rh0funcs[8] =
{
  i_unimp_unknown,		/* blki rh0, */
  i_datai_rh0,
  i_unimp_unknown,		/* blko rh0, */
  i_datao_rh0,
  i_cono_rh0,
  i_coni_rh0,
  i_consz_rh0,
  i_conso_rh0,
};

struct dcb rh0dcb =
{
  NULL, NULL, NULL, NULL,
  &rh0funcs
};

IO_INST(datao,rh0,540)
{
  datao_rh20 (channels[0], ea);
}

IO_INST(datai,rh0,540)
{
  datai_rh20(channels[0], ea);
}

IO_INST(cono,rh0,540)		/* rh20 #0 */
{
  cono_rh20 (channels[0], ea);
}

IO_INST(conso,rh0,540)
{
  if (channels[0]
      && (channels[0]->coni & ea))
    incrpc;
}

IO_INST(consz,rh0,540)
{
  if (channels[0]
      && (channels[0]->coni & ea) == 0)
    incrpc;
}

IO_INST(coni,rh0,540)
{
  coni_rh20 (channels[0], ea);
}

static int kniconihi = 0;
static int kniconilo = 0;
static addr10 pcbbase;		/* port control block address */
unsigned char myether[6];	/* Our host's ethernet address */
static unsigned char broadether[] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

struct ethernet_protocol_queue
{
  int protocol_type;
  addr10 queue_header;
};

static int num_protocols;
static struct ethernet_protocol_queue protocol_queue_headers[16];

static int starting_kni = 0;

void
process_receive_packet ()
{
  unsigned char *packet, *p;
  int protocol;
  int packetlen;
  addr10 queue;
  word36 mem;
  word36 cmdword;
  addr10 fqflink, rqblink, bufptr;
  addr10 bsdaddr, dataddr;
  int bsdlen;
  int xferlen;

  clear_interrupt (0, KNI_BIT);

  /* First, make sure that response queue is free */

  pfetch (pcbbase + 4, mem); /* Get Q interlock */
  if (cne (mem, one36))
    {
      printf ("Rcv: Response queue is locked!\r\n");
      return;			/* Not free, try again later */
    }

  while (1)
    {
      int i;

      packetlen = get_packet (&packet);

      if (packetlen == 0)
	return;

      /* We receive our own broadcasts.  Just ignore them. */
      if (memcmp (packet, broadether, 6) == 0
	  && memcmp (&packet[6], myether, 6) == 0)
	continue;

#if 0
      if (packet[0] != 0xff)
	{
	  printf ("process_receive_packet: got %d bytes: ", packetlen);
	  for (i = 0; i < packetlen; i++)
	    printf ("%02x ", packet[i]);
	  printf ("\r\n");
	}
#endif

      /* Determine which queue to take a buffer from */

      protocol = packet[12] << 8 | packet[13];

      queue = 0;
      for (i = 0; i < num_protocols; i++)
	{
	  if (protocol_queue_headers[i].protocol_type != protocol)
	    continue;

	  queue = protocol_queue_headers[i].queue_header;
	}

      if (!queue)
	queue = pcbbase + 010;	/* Unknown packet queue */

      /* Grab the buffer off of a free queue */

      pfetch (queue + 0, mem);	/* Grab the lock word */
      if (cne (mem, one36))
	{
	  printf ("Free queue is locked!\r\n");
	  continue;
	}

      pfetch (queue + 1, mem);	/* Grab the forward link */
      fqflink = ldb (35, 22, mem);

      if (fqflink == queue + 1)
	continue;		/* Free queue is empty! */

      bufptr = fqflink;

      pfetch (bufptr + 0, mem); /* Grab forward ptr of first entry */
      fqflink = ldb (35, 22, mem);

      pstore (queue + 1, mem);	/* Make queue header point to next entry */

      dpb (35, 22, queue + 1, mem);
      pstore (fqflink + 1, mem); /* Make next entry point at queue head */

      /* Have dequeued an input buffer.  Now, fill it up */

      /* Ethernet dest addr */
      dpb (7, 8, packet[0], mem);
      dpb (15, 8, packet[1], mem);
      dpb (23, 8, packet[2], mem);
      dpb (31, 8, packet[3], mem);
      dpb (35, 4, 0, mem);
      pstore (bufptr + 5, mem); /* Top 4 bytes of dest ethernet addr */

      dpb (7, 8, packet[4], mem);
      dpb (15, 8, packet[5], mem);
      dpb (35, 20, 0, mem);
      pstore (bufptr + 6, mem); /* Bottom two bytes of ethernet addr */

      /* Ethernet source addr */
      dpb (7, 8, packet[6], mem);
      dpb (15, 8, packet[7], mem);
      dpb (23, 8, packet[8], mem);
      dpb (31, 8, packet[9], mem);
      dpb (35, 4, 0, mem);
      pstore (bufptr + 7, mem); /* Top 4 bytes of source ethernet addr */

      dpb (7, 8, packet[10], mem);
      dpb (15, 8, packet[11], mem);
      dpb (35, 20, 0, mem);
      pstore (bufptr + 010, mem); /* Bottom two bytes of ethernet addr */

      /* protocol type */
      dpb (15, 16, 0, mem);
      dpb (23, 8, packet[13], mem);
      dpb (31, 8, packet[12], mem);
      dpb (35, 4, 0, mem);
      pstore (bufptr + 011, mem);

      packetlen -= 14;		/* Subtract ethernet header */
      packetlen += 4;		/* Fake CRC */

      dpb (35, 16, packetlen, mem); /* Store received packet length */
      dpb (19, 20, 0, mem);
      pstore (bufptr + 4, mem);

      /* Now, copy the data */

      /* Pretend we are looking at a BSD */
      bsdaddr = bufptr + 012 - 1;

      for ( ;packetlen > 0; packetlen -= xferlen)
	{
	  unsigned char *xferend;

	  pfetch (bsdaddr + 1, mem); /* Next BSD pointer */
	  bsdaddr = ldb (35, 22, mem);
	  if (!bsdaddr)
	    break;

	  pfetch (bsdaddr + 2, mem); /* BSD length */
	  bsdlen = ldb (35, 22, mem);

	  pfetch (bsdaddr + 0, mem); /* Buffer address */
	  dataddr = ldb (35, 22, mem);

	  xferlen = min (bsdlen, packetlen);

	  p = &packet[14];

	  for (xferend = p + xferlen; p < xferend; dataddr++, p += 4)
	    ic_to_word36 (p, memarray[dataddr]);
	}

      cmdword = zero36;

      if (packetlen > 0)
	{
	  dpb (7, 6, (031 << 1) | 1, cmdword); /* Packet too long error code */

#if 0
	  printf ("Packet too long!  excess = %d, ", packetlen);
	  printf ("%02x-%02x-%02x-%02x-%02x-%02x<-\
%02x-%02x-%02x-%02x-%02x-%02x ",
		  packet[0], packet[1], packet[2], packet[3], packet[4],
		  packet[5], packet[6], packet[7], packet[8], packet[9],
		  packet[10], packet[11]);
	  printf ("Protocol: %02x-%02x\r\n", packet[12], packet[13]);
#endif
	}

      dpb (23, 8, 5, cmdword);	/* Opcode 5 == receive */

      pstore (bufptr + 3, cmdword); /* Install the command word and error code */

      /* Put the block on the end of response queue */

      /* Fetch pointer to last block */
      pfetch (pcbbase + 6, mem);
      rqblink = ldb(35, 22, mem);

      /* Only set this if the queue is currently empty */
      if (rqblink == pcbbase + 5)
	{
	  kniconilo |= 0200;	/* Response queue avail */
	  if (kniconilo & 07)
	    set_interrupt (kniconilo, KNI_BIT);
	}

      /* Setup back link in our block */
      pstore (bufptr + 1, mem);

      /* Setup forward link in our block */
      dpb (3, 4, 0, mem);
      dpb (35, 22, pcbbase + 5, mem);
      pstore (bufptr + 0, mem);

      /* Setup back link from response queue */
      dpb (35, 22, bufptr, mem);
      pstore (pcbbase + 6, mem);

      /* Setup forward pointer from last block */
      pstore (rqblink + 0, mem);
    }
}

static void
receive_ethernet_packet ()
{
  set_interrupt (0, KNI_BIT);
}

static void
init_kni ()
{
  if (!init_ether ())
    return;

  kniconihi = 0400003;		/* Port present, klipa... */
}

static void
send_kni (ceaddr)
     addr10 ceaddr;
{
  word36 mem;
  int totlen;
  int bsdlen;
  addr10 bsdaddr, dataddr;
  int i;
  int flags;
  unsigned char packet[2000], *p;

  pfetch (ceaddr + 3, mem);	/* flags */
  flags = ldb (15, 8, mem);

  pfetch (ceaddr + 4, mem);	/* text length */
  totlen = ldb (35, 16, mem);

  pfetch (ceaddr + 5, mem);	/* protocol type */
  packet[12] = ldb (31, 8, mem);
  packet[13] = ldb (23, 8, mem);

  memcpy (&packet[6], myether, 6);	/* Source address */

  pfetch (ceaddr + 7, mem);	/* Top 4 bytes of dest ethernet addr */
  packet[0] = ldb (7, 8, mem);
  packet[1] = ldb (15, 8, mem);
  packet[2] = ldb (23, 8, mem);
  packet[3] = ldb (31, 8, mem);

  pfetch (ceaddr + 010, mem);	/* Bottom two bytes of ethernet addr */
  packet[4] = ldb (7, 8, mem);
  packet[5] = ldb (15, 8, mem);

  pfetch (ceaddr + 011, mem);	/* Pointer to buffer segment descriptors */
  bsdaddr = ldb (35, 22, mem);

  p = &packet[14];		/* Point to start of data */

  while (bsdaddr != 0)
    {
      pfetch (bsdaddr + 2, mem); /* Get length of this segment */
      bsdlen = ldb (35, 32, mem);

      pfetch (bsdaddr + 0, mem); /* Get address of this segment */
      dataddr = ldb (35, 22, mem);

      for (i = 0; i < bsdlen; i += 4, dataddr++)
	{
	  pfetch (dataddr, mem); /* Get a word of data */
	  *p++ = ldb (7, 8, mem);
	  *p++ = ldb (15, 8, mem);
	  *p++ = ldb (23, 8, mem);
	  *p++ = ldb (31, 8, mem);
	}

      pfetch (bsdaddr + 1, mem); /* Get next bsd */
      bsdaddr = ldb (35, 22, mem);
    }

  totlen += 14;
  send_packet (packet, totlen);

#if 0
  printf ("send_packet: sending %d bytes: ", totlen);
  {
    int i;
    for (i = 0; i < totlen; i++)
      printf ("%02x ", packet[i]);
    printf ("\r\n");
  }
#endif
}

static void
read_station_info (ceaddr)
     addr10 ceaddr;
{
  word36 mem;

  dpb (7, 8, myether[0], mem);
  dpb (15, 8, myether[1], mem);
  dpb (23, 8, myether[2], mem);
  dpb (31, 8, myether[3], mem);
  dpb (35, 4, 0, mem);
  pstore (ceaddr + 4, mem);	/* High order ethernet address */

  dpb (7, 8, myether[4], mem);
  dpb (15, 8, myether[5], mem);
  dpb (35, 20, 0, mem);
  pstore (ceaddr + 5, mem);	/* Low order ethernet address */

  pstore (ceaddr + 6, zero36);	/* Flags */
  pstore (ceaddr + 7, zero36);	/* Version #s */
}

static void
write_station_info (ceaddr)
     addr10 ceaddr;
{
  word36 mem;

  pfetch (ceaddr + 4, mem);	/* High order ethernet address */
  pfetch (ceaddr + 5, mem);	/* Low order ethernet address */

  pfetch (ceaddr + 6, mem);	/* Flags */
  pfetch (ceaddr + 7, mem);	/* Version #s */
}

static void
load_protocol_table (ceaddr)
     addr10 ceaddr;
{
  word36 mem;
  addr10 pttaddr;
  int i;
  int table_pointer;

  pfetch (pcbbase + 015, mem);
  pttaddr = ldb (35, 22, mem);

  table_pointer = 0;
  for (i = 0; i < 16; i++, pttaddr += 3)
    {
      pfetch (pttaddr + 0, mem); /* Fetch enable flag & protocol type */
      if (lt36 (mem))
	{
	  int protocol;
	  addr10 queue;

	  protocol = (ldb (31, 8, mem) << 8) | ldb (23, 8, mem);

	  pfetch (pttaddr + 1, mem); /* Fetch free queue address */
	  queue = ldb (35, 22, mem) - 1;

	  protocol_queue_headers[table_pointer].protocol_type = protocol;
	  protocol_queue_headers[table_pointer++].queue_header = queue;
	}
    }
  num_protocols = table_pointer;
}

static void
load_multicast_table (ceaddr)
     addr10 ceaddr;
{
  word36 mem;
  addr10 mctaddr;
  int i;

  pfetch (pcbbase + 016, mem);
  mctaddr = ldb (35, 22, mem);

  for (i = 0; i < 16; i++)
    {
      pfetch (mctaddr + 1, mem);
      if (biton (35, mem))
	{
#if 0
	  pfetch (mctaddr + 0, mem);
	  printf ("Enabling multicast 0%o,,0%o -- ", upper18(mem), lower18(mem));
	  pfetch (mctaddr + 1, mem);
	  printf ("0%o,,0%o\r\n", upper18(mem), lower18(mem));
#endif
	}
      mctaddr += 2;
    }
}

static void
do_kni_cmd ()
{
  word36 mem;
  addr10 cqflink, rqblink;

  pfetch (pcbbase, mem);

  if (cne (mem, one36))
    {
      printf ("Command queue is locked!\r\n");
      genunimp ();
    }

  pfetch (pcbbase + 1, mem);
  cqflink = ldb (35, 22, mem);

  while (cqflink != pcbbase + 1)
    {
      addr10 ceaddr, ceflink;
      int func;

      ceaddr = cqflink;

      pfetch (ceaddr + 0, mem);
      ceflink = ldb (35, 22, mem);

      /* Fix forward link of queue head */
      cqflink = ceflink;
      dpb (35, 22, cqflink, mem);
      pstore (pcbbase + 1, mem);

      /* Fix back link of next queue entry */
      dpb (35, 22, pcbbase + 1, mem);
      pstore (ceflink + 1, mem);

      /* Command now dequeued, pointer in ceaddr */

      pfetch (ceaddr + 3, mem);	/* Get opcode & flags */

      func = ldb (23, 8, mem);	/* Extract opcode */

      /* Process the command */

      switch (func)
	{
	case 0:			/* Flush command */
	  dpb (7, 6, (033 << 1) | 1, mem); /* Unknown command err */
	  pstore (ceaddr + 3, mem);
	  break;
	case 1:			/* Send packet */
	  send_kni (ceaddr);
	  break;
	case 3:			/* Load protocol table */
	  load_protocol_table (ceaddr);
	  break;
	case 2:			/* Load multicast table */
	  load_multicast_table (ceaddr);
	  break;
	case 8:			/* Read station info */
	  read_station_info (ceaddr);
	  break;
	case 9:			/* Write station info */
	  write_station_info (ceaddr);
	  break;
	default:
	  printf ("Unknown kni function %d at PC %o\r\n", func, pcsection|pc);
	}

      /* Put the block on the end of response queue */

      pfetch (pcbbase + 4, mem); /* Get Q interlock */
      if (cne (mem, one36))
	{
	  printf ("Cmd: Response queue is locked!\r\n");
	  genunimp ();
	}

      /* Fetch pointer to last block */
      pfetch (pcbbase + 6, mem);
      rqblink = ldb(35, 22, mem);

      /* Only set this if the queue is currently empty */
      if (rqblink == pcbbase + 5)
	{
	  kniconilo |= 0200;	/* Response queue avail */
	  if (kniconilo & 07)
	    set_interrupt (kniconilo, KNI_BIT);
	}

      /* Setup back link in our block */
      pstore (ceaddr + 1, mem);

      /* Setup forward link in our block */
      dpb (35, 22, pcbbase + 5, mem);
      pstore (ceaddr + 0, mem);

      /* Setup back link from response queue */
      dpb (35, 22, ceaddr, mem);
      pstore (pcbbase + 6, mem);

      /* Setup forward pointer from last block */
      pstore (rqblink + 0, mem);
    }
}

static void
start_kni ()
{
  word36 icw;
  word36 mem;
  int fcn;
  int wordcnt;
  addr10 physaddr;
  int pia;

  efetch (5 * 4 + 0, &icw);	/* Fetch initial icw from ept */

  fcn = ldb (2, 3, icw);	/* Get func code */

  switch (fcn)
    {
    case 6:			/* fwd data xfer (halt) */
      wordcnt = ldb (13, 11, icw);
      physaddr = ldb (35, 22, icw);
      break;
    default:
      printf ("Initial CCW for kni is bad\r\n");
      genunimp ();
      break;
    }

  if (wordcnt != 3)
    {
      printf ("Initial word count for kni si bad\r\n");
      genunimp ();
    }

  pfetch (physaddr, mem);	/* Get pcb base */
  pcbbase = ldb (35, 22, mem);
  physaddr++;

  pfetch (physaddr, mem);	/* Get PIA */
  pia = ldb (35, 3, mem);
  kniconilo = (kniconilo & ~07) | (pia & 07);
  physaddr++;

  pfetch (physaddr, mem);	/* Get int vector */
}

static io_func_type i_datai_kni;
static io_func_type i_blko_kni;
static io_func_type i_datao_kni;
static io_func_type i_cono_kni;
static io_func_type i_coni_kni;
static io_func_type i_consz_kni;
static io_func_type i_conso_kni;

static io_func_type *knifuncs[8] =
{
  i_unimp_unknown,		/* blki rh0, */
  i_datai_kni,
  i_blko_kni,
  i_datao_kni,
  i_cono_kni,
  i_coni_kni,
  i_consz_kni,
  i_conso_kni,
};

struct dcb knidcb =
{
  NULL, NULL, NULL, NULL,
  &knifuncs
};

IO_INST(cono,kni,564)		/* NIA20 */
{
  if (kniconihi == 0)
    return;			/* Punt, if no port present */

  if (ea & 0111100)
    {
      printf ("Unimplemented control bits in kni: 0%o, pc=0%o\n", ea, pc);
      genunimp ();
    }

  if (ea & 0400000)		/* Port reset */
    {
      kniconihi = 0400003;
      kniconilo = 0;
    }

  if (ea & 0400)		/* Command queue avail */
    {
      if (kniconihi & 020)	/* Enabled? */
	do_kni_cmd ();		/* Yeah, look at the command queue */
    }

  if (ea & 0200)		/* Clear response queue avail */
    {
      kniconilo &= ~0200;
      clear_interrupt (kniconilo, KNI_BIT);
    }

  if (ea & 040)			/* Port disable */
    {
      signal (SIGUSR1, SIG_IGN); /* Disable receive process */
      kniconihi |= 040;		/* Set disable complete */
      kniconihi &= ~020;	/* Clear enable complete */
    }

  if (!(kniconihi & 020)	/* Port not yet enabled? */
      && (ea & 020))		/* and wants port enabled? */
    {
      kniconihi |= 020;		/* Set enable complete */
      kniconihi &= ~040;	/* Clear disable complete */
      /* Enable receive process */
      signal (SIGUSR1, receive_ethernet_packet);
      receive_ethernet_packet (); /* Prime the pump */
    }

  if ((ea ^ kniconilo) & 010)	/* Microprocessor run changing? */
    if (ea & 010		/* And, not yet started? */
	&& starting_kni)	/* And PC == 0? */
      {
	start_kni ();		/* Yup, start up kni */
	starting_kni = 0;
      }
    else
      signal (SIGUSR1, SIG_IGN); /* Disable receive process */

  kniconilo = (kniconilo & ~040017) | (ea & 040017); /* Copy LAR & PI & uproc run */
}

IO_INST(coni,kni,564)
{
  word36 tmp;

  dpb (17, 18, kniconihi, tmp);
  dpb (35, 18, kniconilo, tmp);
  vstore (ea, tmp);
}

IO_INST(consz,kni,564)
{
  if ((kniconilo & ea) == 0)
    incrpc;
}

IO_INST(conso,kni,564)
{
  if (kniconilo & ea)
    incrpc;
}

IO_INST(datao,kni,564)
{
  word36 mem;

  vfetch (ea, mem);

  if (pcflags & PC_USER)
    return;

  if (upper18 (mem) == 0400000	/* Load PC? */
      && lower18 (mem) == 0)	/* to 0? */
    starting_kni = 1;
}

IO_INST(datai,kni,564)
{
  vstore (ea, one36);
}

IO_INST(blko,kni,564)
{
  word36 mem;
  addr10 addr;
  int count;

  vfetch (ea, mem);		/* Get pointer */

  count = upper18 (mem) + 1;
  addr = pcsection | ((lower18 (mem) + 1) & HWORDMASK);

  vfetch (addr, mem);		/* Fetch the output word */

  if (count != 01000000)	/* Count overflow? */
    incrpc;			/* No, skip instruction */

  dpb (17, 18, count, mem);
  dpb (35, 18, addr, mem);

  vstore (ea, mem);
}
