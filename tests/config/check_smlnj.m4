dnl check_smlnj.m4
dnl
dnl COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies.
dnl
dnl @synopsis CHECK_SMLNJ(ACTION-IF-UNKNOWN)
dnl
dnl This macro figures out the location of SML/NJ and its major, minor,
dnl and patch version numbers.  The variables SMLNJ_CMD, SMLNJ_MAJOR_VERSION,
dnl SMLNJ_MINOR_VERSION, and SMLNJ_PATCH_VERSION are set by this macro when
dnl it executes successfully.  This macro also does an AC_SUBST(SMLNJ_CMD).
dnl You can override the version of SML/NJ used by defining the SMLNJ_CMD
dnl variable in the environment.
dnl 
dnl @version $Id: check_smlnj.m4,v 1.1.1.1 2000/06/08 20:29:12 jhr Exp $
dnl @author John Reppy <jhr@research.bell-labs.com>
dnl
AC_DEFUN(CHECK_SMLNJ, [
dnl
dnl first we check for the existence of SML/NJ
dnl
  if test z$SMLNJ_CMD = z ; then
    AC_CHECK_PROGS(SMLNJ_CMD, sml-cm sml, none)
  fi
  if test $SMLNJ_CMD = none; then
    $1
  else
dnl
dnl SML/NJ is installed, so determine the version numbers
dnl
    $SMLNJ_CMD < config/smlnj-version.sml
    eval `cat smlnj-versions`
    rm -f smlnj-versions
    AC_SUBST(SMLNJ_CMD)
  fi
])dnl
