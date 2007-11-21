dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2007 Ths SML/NJ Fellowship.
dnl All rights reserved.
dnl
dnl Configuration for the ml-idl tool
dnl

dnl NOTE: MANTICORE_DIST_VERSION is an M4 variable defined in config/aclocal.m4
dnl
AC_INIT(ml-idl,1.0,http://smlnj.org)

AC_PREREQ(2.59)
AC_CONFIG_AUX_DIR(config)

dnl
dnl include extra macros
dnl
sinclude(config/check_smlnj.m4)
sinclude(config/check_smlnj_heap_suffix.m4)

AC_PREFIX_DEFAULT(/usr/local)

AC_PROG_INSTALL
AC_PATH_PROGS(CPP_CMD, cpp, no, $PATH:/lib)
if test x$CPP_CMD = xno ; then
  AC_MSG_ERROR([no cpp found])
fi

dnl
dnl Figure out the sizes of a few things
dnl
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(char *)

dnl
dnl we want to substitute these sizes into SML code so
dnl add them to the substitutions
dnl
SIZEOF_CHAR=$ac_cv_sizeof_char
SIZEOF_SHORT=$ac_cv_sizeof_short
SIZEOF_INT=$ac_cv_sizeof_int
SIZEOF_LONG=$ac_cv_sizeof_long
SIZEOF_CHAR_P=$ac_cv_sizeof_char_p
AC_SUBST(SIZEOF_CHAR)
AC_SUBST(SIZEOF_SHORT)
AC_SUBST(SIZEOF_INT)
AC_SUBST(SIZEOF_LONG)
AC_SUBST(SIZEOF_CHAR_P)

dnl
dnl allow the user to override the location of the SML/NJ library
dnl
SMLNJ_LIB_DIR=""
AC_ARG_WITH(smlnj-lib,
  [  --with-smlnj-lib=PATH   specify location of SML/NJ Library],
  SMLNJ_LIB_DIR=$with_smlnj_lib)
if test x$with_smlnj_lib = xyes -o x$with_smlnj_lib = xno ; then
  AC_MSG_ERROR([--with-smlnj-lib option must specify directory argument])
elif test x$with_smlnj_lib != x -a ! -r $SMLNJ_LIB_DIR/Util/smlnj-lib.cm ; then
  AC_MSG_ERROR([SML/NJ Library not found at $SMLNJ_LIB_DIR])
elif test x$SMLNJ_LIB_DIR != x ; then
dnl
dnl verify that $SMLNJ_LIB_DIR is an absolute path
dnl
  case $SMLNJ_LIB_DIR in
    /*) ;;
    *) AC_MSG_ERROR([--with-smlnj-lib argument must be absolute]) ;;
  esac
fi
SMLNJ_LIB_CM="smlnj-lib.cm"
PP_LIB_CM="pp-lib.cm"
if test x$SMLNJ_LIB_DIR != x ; then
  SMLNJ_LIB_CM=$SMLNJ_LIB_DIR/Util/$SMLNJ_LIB_CM
  PP_LIB_CM=$SMLNJ_LIB_DIR/PP/$PP_LIB_CM
fi
AC_SUBST(SMLNJ_LIB_CM)
AC_SUBST(PP_LIB_CM)

dnl
dnl allow user to specify location of Moby
dnl
AC_ARG_WITH(moby,
  [  --with-moby=PATH        specify root of Moby distribution],
  MOBY_DIR=$with_moby)
if test x$with_moby = xyes -o x$with_moby = xno ; then
  AC_MSG_ERROR([--with-moby option must specify directory argument])
elif test x$with_moby != x -a ! -r $MOBY_DIR/src/mbx-lib/sources.cm ; then
  AC_MSG_ERROR([MBX library not found at $MOBY_DIR/src/mbx-lib])
elif test x$MOBY_DIR != x ; then
dnl
dnl verify that MOBY_DIR is absolute
dnl
  case $MOBY_DIR in
    /*) ;;
    *) AC_MSG_ERROR([--with-moby argument must be absolute]) ;;
  esac
  BUILD_ML_FLAGS="-DSUPPORT_MOBY"
fi
AC_SUBST(MOBY_DIR)

dnl ******************** SML/NJ Compiler configuration ********************

dnl
dnl
dnl check that we have a compatible version of SML/NJ
dnl
CHECK_SMLNJ([AC_MSG_ERROR([must have SML/NJ installed])])
if test $SMLNJ_MAJOR_VERSION -eq 110 \
  -a $SMLNJ_MINOR_VERSION -ge 67
then
  USING_NEW_CM="TRUE"
else
  AC_MSG_ERROR([installation requires either version 110.67+ of SML/NJ])
fi

dnl
dnl determine the heap-image suffix
dnl
CHECK_SMLNJ_HEAP_SUFFIX([
  AC_MSG_ERROR([unsupported configuration ${host_cpu}-${host_os}])])


dnl
dnl define various directory paths
dnl
LOCAL_ROOT=`pwd`
LOCAL_SRCDIR=$LOCAL_ROOT/src
LOCAL_BINDIR=$LOCAL_ROOT/bin
HEAP_IMAGE_DIR=$LOCAL_BINDIR/.heap
INSTALL_SMLNJ_WRAPPER=$LOCAL_BINDIR/install-sml-wrapper.sh

AC_SUBST(LOCAL_SRCDIR)
AC_SUBST(LOCAL_BINDIR)
AC_SUBST(HEAP_IMAGE_DIR)
AC_SUBST(INSTALL_SMLNJ_WRAPPER)


dnl ******************** Output ********************

dnl
dnl write output
dnl
AC_CONFIG_FILES(\
dnl ***** Makefiles *****
  Makefile \
  src/Makefile \
dnl ***** CM files *****
  src/sources.cm:src/sources_cm.in
  src/Main/sources.cm:src/Main/sources_cm.in
  src/TargetMoby/sources.cm:src/TargetMoby/sources_cm.in)

dnl
dnl shell scripts
dnl
AC_CONFIG_FILES([
  bin/install-sml-wrapper.sh:config/install-sml-wrapper_sh.in
], [chmod +x bin/install-sml-wrapper.sh])

AC_OUTPUT
