#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

# The first parameter is the architecture
# The second parameter is the stage name


SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envvars.sh

CACHE="${BOOTSTRAP_CACHE}"
ARCH=64

find ${CACHE}

# I will use the name of the image to determine the vm version (because file name is in the format Pharo7.0.0-rc1)
#
# WARNING: I'm assuming CACHE=bootstrap-cache
#
TEST_NAME_PREFIX=$(basename `find ${CACHE} -name "Pharo*.zip" | head -n 1` | cut -d'-' -f 1-2)

# Extract the VM version from the image file version, avoiding going to git to extract the tags
# This is handy in later stages of the build process when no repository is available, e.g., to run the tests
# Input: Pharo11.0-PR-64bit-7264e14.zip
# Output: 110
# Works by 
#  - taking the entire name,
#  - removing the suffix after the first dot
#  - removing the prefix "Pharo"
TEST_VM_VERSION=`echo ${TEST_NAME_PREFIX} | cut -d'.' -f 1 | cut -d'-' -f 1 | cut -c6-`0

TEST_VM_KIND="vm"

${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/getPharoVM.sh ${TEST_VM_VERSION} ${TEST_VM_KIND} ${ARCH}

IMAGE_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-${ARCH}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name Pharo*-${ARCH}bit-*.image)
CHANGES_FILE=$(find . -name Pharo*-${ARCH}bit-*.changes)
				
cp ${CACHE}/*.sources .
mv $IMAGE_FILE Pharo.image
mv $CHANGES_FILE Pharo.changes

${BOOTSTRAP_REPOSITORY:-.}/tests/run-cli-tests.sh