#!/usr/bin/env bash
#
# Set up environment variables used by the various
# bootstrap stages
#
# Required Input environment variables:
#
# - BUILD_NUMBER
# - BOOTSTRAP_ARCH
#
# Optional input environment variables:
#
# - BOOTSTRAP_CACHE		# Working directory during building
# - BOOTSTRAP_REPOSITORY	# Location of the pharo git clone
# - BOOTSTRAP_VMTARGET		# Location of the target VM
#				  This can be set when a custom VM is to be 
#				  used during bootstrap.
#
if [ -z "${BUILD_NUMBER}" ]
then
    echo "BUILD_NUMBER not specified, exiting"
    exit 1
fi

if [ -z "${BOOTSTRAP_ARCH}" ]
then
    echo "BOOTSTRAP_ARCH not specified, exiting"
    exit 1
fi

if [ -z "${BOOTSTRAP_REPOSITORY}" ]
then
  ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." ; pwd -P)"
  export BOOTSTRAP_REPOSITORY="${ROOT_DIR}"
else
  ROOT_DIR="$(pwd -P)"
fi

if [ -z "${BOOTSTRAP_CACHE}" ]
then
  BOOTSTRAP_CACHE=${ROOT_DIR}/bootstrap-cache
fi
export BOOTSTRAP_CACHE

# This is the VM used to bootstrap, i.e. the target VM

# Flags to run the image
IMAGE_FLAGS="--no-default-preferences"

# Include pharo version 
. $(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)/envversion.sh

if [ -z "${BOOTSTRAP_VMTARGET}" ]
then
    VM="${BOOTSTRAP_CACHE}/vmtarget/pharo --headless"
else
    VM="${BOOTSTRAP_VMTARGET}/pharo --headless"
fi
