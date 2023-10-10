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
if [ -z ${BUILD_NUMBER+x} ]
then
    echo "BUILD_NUMBER not specified, defaulting to 0"
    export BUILD_NUMBER=0
fi

if [ -z ${BOOTSTRAP_ARCH+x} ]
then
    echo "BOOTSTRAP_ARCH not specified, defaulting to 64"
    export BOOTSTRAP_ARCH=64
fi

if [ -z ${BOOTSTRAP_REPOSITORY+x} ]
then
  export BOOTSTRAP_REPOSITORY="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." ; pwd -P)"
fi

ROOT_DIR="$(pwd -P)/build"

if [ -z ${BOOTSTRAP_CACHE+x} ]
then
  BOOTSTRAP_CACHE=${ROOT_DIR}/bootstrap-cache
fi
export BOOTSTRAP_CACHE

# This is the place where we put files we need to download. i.e. 
if [ -z ${BOOTSTRAP_DOWNLOADS+x} ]
then
  BOOTSTRAP_DOWNLOADS=${ROOT_DIR}/bootstrap-downloads
fi
export BOOTSTRAP_DOWNLOADS

# This is the VM used to bootstrap, i.e. the target VM
if [ -z ${BOOTSTRAP_VMTARGET+x} ]
then
    VM="${BOOTSTRAP_DOWNLOADS}/vmtarget/pharo --headless"
else
    VM="${BOOTSTRAP_VMTARGET} --headless"
fi

#This is the VM to use during the bootstrap (initialization of the small image)
VM_BOOTSTRAP="${BOOTSTRAP_DOWNLOADS}/vmBootstrap/pharo --headless"

# Flags to run the image
IMAGE_FLAGS="--no-default-preferences"

# Include pharo version 
. $(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)/envversion.sh

