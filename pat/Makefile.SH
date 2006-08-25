: Makefile.SH generated from Jmake.tmpl and Jmakefile [jmake 3.5 PL0]
: $X-Id: Jmake.tmpl 8 2006-08-25 22:27:18Z rmanfredi $

case $CONFIG in
'')
	if test -f config.sh; then TOP=.;
	elif test -f ../config.sh; then TOP=..;
	elif test -f ../../config.sh; then TOP=../..;
	elif test -f ../../../config.sh; then TOP=../../..;
	elif test -f ../../../../config.sh; then TOP=../../../..;
	else
		echo "Can't find config.sh."; exit 1
	fi
	. $TOP/config.sh
	;;
esac
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac
CURRENT=pat
DIR=`echo $CURRENT/ | sed -e 's/\.\///g'`
echo "Extracting ${DIR}Makefile (with variable substitutions)"

INSTALL=`echo $install | sed -e 's,\./i,\$(TOP)/i,'`
INSTALLDIR=`echo $installdir | sed -e 's,\./i,\$(TOP)/i,'`
DATE=`date`

$spitshell >Makefile <<!GROK!THIS!
########################################################################
# Makefile generated from Makefile.SH on $DATE

SHELL = /bin/sh
JMAKE = jmake
TOP = ..
CURRENT = $CURRENT
DIR = $DIR
INSTALL = $INSTALL
INSTALLDIR = $INSTALLDIR

########################################################################
# Parameters set by Configure -- edit config.sh if changes are needed

CTAGS = ctags
JCPPFLAGS = $cppflags
L = $manext
MANSRC = $installmansrc
MV = $mv
RM = $rm -f
SCRIPTDIR = $installscript
SED = $sed

!GROK!THIS!
$spitshell >>Makefile <<'!NO!SUBS!'
########################################################################
# Jmake rules for building libraries, programs, scripts, and data files
# $X-Id: Jmake.rules 8 2006-08-25 22:27:18Z rmanfredi $

########################################################################
# Start of Jmakefile

# $X-Id: Jmakefile 9 2006-08-25 22:31:50Z rmanfredi $
#
#  Copyright (c) 1991-1997, 2004-2006, Raphael Manfredi
#
#  You may redistribute only under the terms of the Artistic Licence,
#  as specified in the README file that comes with the distribution.
#  You may reuse parts of this distribution only within the terms of
#  that same Artistic Licence; a copy of which may be found at the root
#  of the source tree for dist 4.0.
#
# $X-Log: Jmakefile,v $
# Revision 3.0.1.3  1995/07/24  09:57:21  ram
# patch56: was missing an entry for the patlog program
#
# Revision 3.0.1.2  1994/01/24  13:42:48  ram
# patch16: added dependency generation stage
#
# Revision 3.0.1.1  1993/08/24  12:09:05  ram
# patch3: added patnotify and patsnap
#
# Revision 3.0  1993/08/18  12:10:32  ram
# Baseline for dist 3.0 netwide release.
#

BASE = // cil base diff make clean col name ftp send \
	index post notify snap log

PAT = \
	pat \
	patcil \
	patbase \
	patdiff \
	patmake \
	patclean \
	patcol \
	patname \
	patftp \
	patsend \
	patindex \
	patpost \
	patnotify \
	patsnap \
	patlog 

all:: $(PAT)

local_realclean::
	$(RM) $(PAT)

pat: pat.SH
	/bin/sh pat.SH

patcil: patcil.SH
	/bin/sh patcil.SH

patbase: patbase.SH
	/bin/sh patbase.SH

patdiff: patdiff.SH
	/bin/sh patdiff.SH

patmake: patmake.SH
	/bin/sh patmake.SH

patclean: patclean.SH
	/bin/sh patclean.SH

patcol: patcol.SH
	/bin/sh patcol.SH

patname: patname.SH
	/bin/sh patname.SH

patftp: patftp.SH
	/bin/sh patftp.SH

patsend: patsend.SH
	/bin/sh patsend.SH

patindex: patindex.SH
	/bin/sh patindex.SH

patpost: patpost.SH
	/bin/sh patpost.SH

patnotify: patnotify.SH
	/bin/sh patnotify.SH

patsnap: patsnap.SH
	/bin/sh patsnap.SH

patlog: patlog.SH
	/bin/sh patlog.SH


local_install:: $(PAT)
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(SCRIPTDIR) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(SCRIPTDIR)); \
	for i in $(PAT); do \
		(set -x; $(INSTALL) -c -m 555 \
			$$i $(INSTALL_PREFIX)$(SCRIPTDIR)) || exit 1;\
	done

local_deinstall::
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in $(PAT); do \
		(set -x; $(RM) $(INSTALL_PREFIX)$(SCRIPTDIR)/$$i); \
	done

local_install.man:: pat.man
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(MANSRC) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(MANSRC)); \
	$(INSTALL) -c -m 444 pat.man $(INSTALL_PREFIX)$(MANSRC)/pat.$(L)

local_deinstall.man::
	$(RM) $(INSTALL_PREFIX)$(MANSRC)/pat.$(L)

PATSH = \
	pat.SH \
	patcil.SH \
	patbase.SH \
	patdiff.SH \
	patmake.SH \
	patclean.SH \
	patcol.SH \
	patname.SH \
	patftp.SH \
	patsend.SH \
	patindex.SH \
	patpost.SH \
	patnotify.SH \
	patsnap.SH \
	patlog.SH 

depend::
	($(SED) '/^# DO NOT DELETE/q' Makefile && \
	grep '^\$$grep' $(PATSH) | \
	$(SED) -e "s/^.*' \([^ ]*\) >>\([^ ]*\)/\2: \1/" \
	) > Makefile.new
	cp Makefile Makefile.bak
	cp Makefile.new Makefile
	$(RM) Makefile.new

########################################################################
# Common rules for all Makefiles -- do not edit

all::

clean: local_clean
realclean: local_realclean
clobber: local_clobber

local_clean::
	if test -f core; then $(RM) core; fi
	$(RM) *~ *.o

local_realclean:: local_clean

local_clobber:: local_realclean
	$(RM) Makefile config.sh

install:: local_install
install.man:: maybe_install.man
deinstall:: local_deinstall
deinstall.man:: maybe_deinstall.man

install.man-yes: local_install.man
install.man-no:
deinstall.man-yes: local_deinstall.man
deinstall.man-no:

!NO!SUBS!
case "$installmansrc" in
'') man=no;;
*) man=yes;;
esac
$spitshell >>Makefile <<!GROK!THIS!
maybe_install.man: install.man-$man
maybe_deinstall.man: deinstall.man-$man
!GROK!THIS!
$spitshell >>Makefile <<'!NO!SUBS!'

Makefile.SH: Jmakefile
	-@if test -f $(TOP)/.package; then \
		if test -f Makefile.SH; then \
			echo "	$(RM) Makefile.SH~; $(MV) Makefile.SH Makefile.SH~"; \
			$(RM) Makefile.SH~; $(MV) Makefile.SH Makefile.SH~; \
		fi; \
		echo "	$(JMAKE) -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT)" ; \
		$(JMAKE) -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT) ; \
	else touch $@; fi

Makefile: Makefile.SH
	/bin/sh Makefile.SH

tags::
	$(CTAGS) -w *.[ch]
	$(CTAGS) -xw *.[ch] > tags

local_clobber::
	$(RM) tags

########################################################################
# Empty rules for directories with no sub-directories -- do not edit

local_install::
	@echo "install in $(CURRENT) done."

local_deinstall::
	@echo "deinstall in $(CURRENT) done."

local_install.man::
	@echo "install.man in $(CURRENT) done."

local_deinstall.man::
	@echo "deinstall.man in $(CURRENT) done."

Makefiles::

Makefiles.SH::

!NO!SUBS!
chmod 644 Makefile
$eunicefix Makefile

