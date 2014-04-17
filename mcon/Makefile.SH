: Makefile.SH generated from Jmake.tmpl and Jmakefile [jmake 3.5-174]
: $X-Id: Jmake.tmpl 156 2012-12-19 16:44:39Z rmanfredi $

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
CURRENT=mcon
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
MV = $mv
PRIVLIB = $installprivlib
RM = $rm -f
SCRIPTDIR = $installscript
SED = $sed

########################################################################
# Automatically generated parameters -- do not edit

SUBDIRS = man
BIN =  metaconfig metalint metaxref

!GROK!THIS!
$spitshell >>Makefile <<'!NO!SUBS!'
########################################################################
# Jmake rules for building libraries, programs, scripts, and data files
# $X-Id: Jmake.rules 174 2014-04-05 08:19:48Z rmanfredi $

########################################################################
# Start of Jmakefile

# $X-Id: Jmakefile 177 2014-04-08 10:42:18Z rmanfredi $
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
# Revision 3.0.1.4  1995/07/25  13:23:09  ram
# patch56: installs the GNU configure-like front-end to Configure
#
# Revision 3.0.1.3  1994/01/24  13:42:29  ram
# patch16: added dependency generation stage
#
# Revision 3.0.1.2  1993/11/02  10:40:01  ram
# patch14: now invokes perload explicitely via perl executable
#
# Revision 3.0.1.1  1993/08/25  08:46:44  ram
# patch6: split unit installation to avoid shell command line overflow
#
# Revision 3.0  1993/08/18  12:04:40  ram
# Baseline for dist 3.0 netwide release.
#

all:: Glossary

local_realclean::
	$(RM) Glossary

.FORCE:

Glossary: .FORCE
	chmod +x makegloss
	./makegloss

all:: makegloss

local_realclean::
	$(RM) makegloss

makegloss: makegloss.SH
	/bin/sh makegloss.SH


local_install::
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for dir in $(PRIVLIB) $(PRIVLIB)/U; do	\
		(set -x; test -d $(INSTALL_PREFIX)$$dir || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$$dir);	\
	done

local_deinstall::
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for dir in $(PRIVLIB) $(PRIVLIB)/U; do \
		(set -x; test -d $$dir && $(RM) -r $(INSTALL_PREFIX)$$dir); \
	done

local_install:: Glossary
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(PRIVLIB) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(PRIVLIB)); \
	$(INSTALL) -c -m 444 Glossary $(INSTALL_PREFIX)$(PRIVLIB)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(PRIVLIB)/Glossary

local_install:: makegloss
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(PRIVLIB) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(PRIVLIB)); \
	$(INSTALL) -c -m 555  makegloss $(INSTALL_PREFIX)$(PRIVLIB)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(PRIVLIB)/makegloss

local_install:: configure
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(PRIVLIB) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(PRIVLIB)); \
	$(INSTALL) -c -m 555  configure $(INSTALL_PREFIX)$(PRIVLIB)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(PRIVLIB)/configure

local_install:: U/[a-d]*.U
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(PRIVLIB)/U || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(PRIVLIB)/U); \
	for i in U/[a-d]*.U; do \
		(set -x; $(INSTALL) -c -m 444 \
			$$i $(INSTALL_PREFIX)$(PRIVLIB)/U) || exit 1;\
	done

local_deinstall::
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in U/[a-d]*.U; do \
		(set -x; $(RM) $(INSTALL_PREFIX)$(PRIVLIB)/U/$$i); \
	done

local_install:: U/[A-Ze-z]*.U
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(PRIVLIB)/U || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(PRIVLIB)/U); \
	for i in U/[A-Ze-z]*.U; do \
		(set -x; $(INSTALL) -c -m 444 \
			$$i $(INSTALL_PREFIX)$(PRIVLIB)/U) || exit 1;\
	done

local_deinstall::
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in U/[A-Ze-z]*.U; do \
		(set -x; $(RM) $(INSTALL_PREFIX)$(PRIVLIB)/U/$$i); \
	done

all:: metaconfig mconfig

local_realclean::
	$(RM) metaconfig mconfig

all:: mconfig

local_realclean::
	$(RM) mconfig

mconfig: mconfig.SH
	/bin/sh mconfig.SH

metaconfig: mconfig
	perl $(TOP)/bin/perload -o mconfig > $@
	chmod +rx $@

local_install:: metaconfig
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(SCRIPTDIR) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(SCRIPTDIR)); \
	$(INSTALL) -c -m 555  metaconfig $(INSTALL_PREFIX)$(SCRIPTDIR)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(SCRIPTDIR)/metaconfig

all:: metalint mlint

local_realclean::
	$(RM) metalint mlint

all:: mlint

local_realclean::
	$(RM) mlint

mlint: mlint.SH
	/bin/sh mlint.SH

metalint: mlint
	perl $(TOP)/bin/perload -o mlint > $@
	chmod +rx $@

local_install:: metalint
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(SCRIPTDIR) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(SCRIPTDIR)); \
	$(INSTALL) -c -m 555  metalint $(INSTALL_PREFIX)$(SCRIPTDIR)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(SCRIPTDIR)/metalint

all:: metaxref mxref

local_realclean::
	$(RM) metaxref mxref

all:: mxref

local_realclean::
	$(RM) mxref

mxref: mxref.SH
	/bin/sh mxref.SH

metaxref: mxref
	perl $(TOP)/bin/perload -o mxref > $@
	chmod +rx $@

local_install:: metaxref
	@case '${MFLAGS}' in *[i]*) set +e;; esac;		\
	(set -x; test -d $(INSTALL_PREFIX)$(SCRIPTDIR) || \
		$(INSTALLDIR) $(INSTALL_PREFIX)$(SCRIPTDIR)); \
	$(INSTALL) -c -m 555  metaxref $(INSTALL_PREFIX)$(SCRIPTDIR)

local_deinstall::
	$(RM) $(INSTALL_PREFIX)$(SCRIPTDIR)/metaxref

BINFILES = \
	mconfig \
	mlint \
	mxref 

BINSH = \
	mconfig.SH \
	mlint.SH \
	mxref.SH 

depend:: local_depend
local_depend::
	($(SED) '/^# DO NOT DELETE/q' Makefile && \
	grep '^\$$grep' $(BINSH) | \
	$(SED) -e "s/^.*' \([^ ]*\) >>\([^ ]*\)/\2: \1/" \
	) > Makefile.new
	cp Makefile Makefile.bak
	cp Makefile.new Makefile
	$(RM) Makefile.new

mconfig mlint mxref: $(TOP)/revision.h

########################################################################
# Common rules for all Makefiles -- do not edit

all::

clean: sub_clean local_clean
realclean: sub_realclean local_realclean
clobber: sub_clobber local_clobber

local_clean::
	if test -f core; then $(RM) core; fi
	$(RM) *~ *.o

local_realclean:: local_clean

local_clobber:: local_realclean
	$(RM) Makefile config.sh

install:: local_install sub_install
	@exit 0

install.man:: maybe_install.man sub_install.man
deinstall:: sub_deinstall local_deinstall
deinstall.man:: sub_deinstall.man maybe_deinstall.man

install.man-no:
deinstall.man-no:

maybe_install.man: install.man-no
maybe_deinstall.man: deinstall.man-no

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

depend::

########################################################################
# Rules for building in sub-directories -- do not edit

subdirs:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo $(VERB) "in $(DIR)$$i..."; \
			$(MAKE) $(MFLAGS) $(FLAGS) $(TARGET)) || exit 1; \
	done

sub_install::
	@$(MAKE) subdirs TARGET=install VERB="Installing" FLAGS=

sub_deinstall::
	@$(MAKE) subdirs TARGET=deinstall VERB="Deinstalling" FLAGS=
	@echo "Back to $(CURRENT) for "deinstall...

sub_install.man::
	@$(MAKE) subdirs TARGET=install.man VERB="Installing man pages" FLAGS=

sub_deinstall.man::
	@$(MAKE) subdirs TARGET=deinstall.man VERB="Deinstalling man pages" FLAGS=
	@echo "Back to $(CURRENT) for "deinstall.man...

sub_clean::
	@$(MAKE) subdirs TARGET=clean VERB="Cleaning" FLAGS=
	@echo "Back to $(CURRENT) for "clean...

sub_realclean::
	@$(MAKE) subdirs TARGET=realclean VERB="Real cleaning" FLAGS=
	@echo "Back to $(CURRENT) for "realclean...

sub_clobber::
	@$(MAKE) subdirs TARGET=clobber VERB="Clobbering" FLAGS=
	@echo "Back to $(CURRENT) for "clobber...

tag::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "Tagging" "in $(DIR)$$i..."; \
			$(MAKE) $(MFLAGS)  tag) || exit 1; \
	done

Makefiles::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS);\
	do \
		echo "Making "Makefiles" in $(DIR)$$i..."; \
		(cd $$i || exit 1; \
		if test ! -f Makefile; then /bin/sh Makefile.SH; fi; \
		$(MAKE) $(MFLAGS) Makefiles) || exit 1;\
	done

Makefiles.SH:: Makefile.SH
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS);\
	do \
		case "$(DIR)$$i/" in \
		*/*/*/*/) newtop=../../../..;; \
		*/*/*/) newtop=../../..;; \
		*/*/) newtop=../..;; \
		*/) newtop=..;; \
		esac; \
		case "$(TOP)" in \
		/*) newtop="$(TOP)" ;; \
		esac; \
		echo "Making Makefiles.SH in $(DIR)$$i..."; \
		(cd $$i || exit 1; \
			if test -f Jmakefile; then \
				$(MAKE) $(MFLAGS) -f ../Makefile \
					Makefile TOP=$$newtop CURRENT=$(DIR)$$i && \
				$(MAKE) $(MFLAGS) Makefiles.SH; \
			fi; \
		) || exit 1; \
	done

all::
	@$(MAKE) subdirs TARGET=all VERB="Making all" FLAGS=

local_install::
local_deinstall::
local_install.man::
local_deinstall.man::

########################################################################
# Dependencies generated by make depend
# DO NOT DELETE THIS LINE -- make depend relies on it

# Put nothing here or make depend will gobble it up
.FORCE_DEPEND::
	@echo "You must run 'make depend' in $(TOP) first."; exit 1
!NO!SUBS!
chmod 644 Makefile
$eunicefix Makefile

