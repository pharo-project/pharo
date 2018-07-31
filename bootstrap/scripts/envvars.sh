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

CACHE="${BOOTSTRAP_CACHE:-${ROOT_DIR}/bootstrap-cache}"

# Ensure that BOOTSTRAP_REPOSITORY is propagated
# This is the VM used to bootstrap, i.e. the target VM
VM="${ROOT_DIR}/vmtarget/pharo --headless"
