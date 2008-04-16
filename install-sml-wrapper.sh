#!/bin/sh
#
# COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
#
# Generated automatically from install-sml-wrapper_sh.in by configure.
#
# a general-purpose script for installing executable wrappers for SML/NJ
# programs.
#
#	install-sml-wrapper.sh <heap-image> <install-dir>

INSTALL="/usr/bin/install -c"
INSTALL_DATA="${INSTALL} -m 644"
HEAP_SUFFIX="x86-linux"

if test $# -ne 2 ; then
  echo "usage: install-sml-wrapper.sh <heap-image> <install-dir>"
  exit 1
fi

TARGET=$1
INSTALL_DIR=$2
HEAP_IMAGE=$TARGET.$HEAP_SUFFIX
INSTALL_TARGET=$INSTALL_DIR/$TARGET
INSTALL_HEAP_IMAGE=$INSTALL_DIR/.heap/$TARGET

if test ! -f $HEAP_IMAGE ; then
  echo "heap image $HEAP_IMAGE not found"
  exit 1
fi

# create the wrapper script
#
cat > $TARGET <<XXXX
#!/bin/sh
#
exec /home/adrassi/smlnj/bin/sml @SMLload=$INSTALL_HEAP_IMAGE \$@
XXXX

#install the script and heap image
#
$INSTALL $TARGET $INSTALL_TARGET || exit 1
$INSTALL_DATA -D $HEAP_IMAGE $INSTALL_DIR/.heap/$HEAP_IMAGE || exit 1

# remove the local copy of the script
rm -f $TARGET

exit 0
