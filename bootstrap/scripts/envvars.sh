#!/bin/bash
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
    echo "BOOTSTRAP_ARCH not specified, existing"
    exit 1
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

CACHE="${BOOTSTRAP_CACHE:-${ROOT_DIR}/bootstrap-cache}"
REPOSITORY="${BOOTSTRAP_REPOSITORY:-${ROOT_DIR}}"
# Ensure that BOOTSTRAP_REPOSITORY is propagated
export BOOTSTRAP_REPOSITORY="${REPOSITORY}"
# This is the VM used to bootstrap, i.e. the target VM
VM="${REPOSITORY}/vmtarget/pharo --headless"
