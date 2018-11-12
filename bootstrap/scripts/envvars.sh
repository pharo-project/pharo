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
# - BOOTSTRAP_CACHE
# - BOOTSTRAP_REPOSITORY
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

PHARO_NAME_PREFIX="Pharo`git describe --long --tags | cut -d'-' -f 1-2 | cut -c 2-`"
PHARO_VM_VERSION=`git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//'`

# Ensure that BOOTSTRAP_REPOSITORY is propagated
# This is the VM used to bootstrap, i.e. the target VM
VM="${BOOTSTRAP_CACHE}/vmtarget/pharo --headless"

# Flags to run the image
IMAGE_FLAGS="--no-default-preferences"
