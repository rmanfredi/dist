/*
 * This is a prototypical patchlevel.h file. Only the line defining
 * the PATCHLEVEL symbol is taken into account when patching, so be
 * sure to make changes to this file ONLY when you start a new release
 * and, of course, before running patcil...
 *
 * This file must appear in your MANIFEST.new, but it will never be
 * checked in by the pat tools. It is automatically updated when a new
 * patch is issued.
 *
 * When using the '-n' option in some pat* scripts, this file is
 * taken as a timestamp. So it is best to avoid manual editing unless
 * you know what you are doing.
 */

/*
 * $Id$
 *
 *  Copyright (c) 1991-1997, 2004-2006, Raphael Manfredi
 *  
 *  You may redistribute only under the terms of the Artistic Licence,
 *  as specified in the README file that comes with the distribution.
 *  You may reuse parts of this distribution only within the terms of
 *  that same Artistic Licence; a copy of which may be found at the root
 *  of the source tree for dist 4.0.
 *
 * $Log: patchlevel.h,v $
 * Revision 3.0  1993/08/18  12:10:39  ram
 * Baseline for dist 3.0 netwide release.
 *
 */

#define VERSION 3.0			/* For instance */
#define PATCHLEVEL 0		/* This line is a mandatory */
