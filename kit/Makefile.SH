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
CURRENT=kit
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

########################################################################
# Automatically generated parameters -- do not edit

SCRIPTS =  \$(SCRIPT)

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
# Revision 3.0.1.2  1994/04/22  09:35:35  ram
# patch23: new kitpost script
#
# Revision 3.0.1.1  1994/01/24  13:42:21  ram
# patch16: added dependency generation stage
#
# Revision 3.0  1993/08/18  12:04:21  ram
# Baseline for dist 3.0 netwide release.
#

SCRIPT = makedist manifake kitsend kitpost

all:: $(SCRIPT)

local_realclean::
	$(RM) $(SCRIPT)

makedist: makedist.SH
	/bin/sh makedist.SH

manifake: manifake.SH
	/bin/sh manifake.SH

kitsend: kitsend.SH
	/bin/sh kitsend.SH

kitpost: kitpost.SH
	/bin/sh kitpost.SH


local_install:: $(SCRIPTS) $(LSCRIPTS)
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(SCRIPTDIR) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(SCRIPTDIR)); \
	for file in $(SCRIPTS) $(LSCRIPTS); do \
		(set -x; \
		$(INSTALL) -c -m 555 $$file $(INSTALL_PREFIX)$(SCRIPTDIR)) || \
		exit 1; \
	done

local_deinstall::
	@for file in $(SCRIPTS) $(LSCRIPTS); do \
		case '${MFLAGS}' in *[i]*) set +e;; esac; \
		(set -x; $(RM) $(INSTALL_PREFIX)$(SCRIPTDIR)/$$file); \
	done

local_install.man::
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(MANSRC) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(MANSRC)); \
	for file in $(SCRIPTS); do \
		if test -f $$file.man; then \
			(set -x; \
			$(INSTALL) -c -m 444 $$file.man \
				$(INSTALL_PREFIX)$(MANSRC)/$$file.$(L)) || \
				exit 1; \
		fi; \
	done

local_deinstall.man::
	case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for file in $(SCRIPTS); do \
		(set -x; $(RM) $(INSTALL_PREFIX)$(MANSRC)/$$file.$(L)); \
	done

local_install:: makeSH
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(SCRIPTDIR) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(SCRIPTDIR)); \
	$(INSTALL) -c -m 555  makeSH $(INSTALL_PREFIX)$(SCRIPTDIR)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(SCRIPTDIR)/makeSH

local_install.man:: makeSH.man
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(MANSRC) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(MANSRC)); \
	$(INSTALL) -c -m 444 makeSH.man $(INSTALL_PREFIX)$(MANSRC)/makeSH.$(L)

local_deinstall.man::
	$(RM) $(INSTALL_PREFIX)$(MANSRC)/makeSH.$(L)

SCRIPTSH = \
	makedist.SH \
	manifake.SH \
	kitsend.SH \
	kitpost.SH 

depend::
	($(SED) '/^# DO NOT DELETE/q' Makefile && \
	grep '^\$$grep' $(SCRIPTSH) | \
	$(SED) -e "s/^.*' \([^ ]*\) >>[ ]*\([^ ]*\)/\2: \1/" \
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

