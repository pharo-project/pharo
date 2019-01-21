#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

# The first parameter is the architecture
# The second parameter is the stage name

CACHE="${BOOTSTRAP_CACHE:-bootstrap-cache}"

find ${CACHE}

# I will use the name of the image to determine the vm version (because file name is in the format Pharo7.0.0-rc1)
#
# WARNING: I'm assuming CACHE=bootstrap-cache
# WARNING: If you change this, you will need to change "runKernelTests.sh" too
#
TEST_NAME_PREFIX=$(find ${CACHE} -name "Pharo*.zip" | head -n 1 | cut -d'/' -f 2 | cut -d'-' -f 1-2)
#TEST_VM_VERSION=$(echo "${TEST_NAME_PREFIX}" | cut -d'-' -f 1| cut -c 6- | cut -d'.' -f 1-2 | sed 's/\.//')
TEST_VM_VERSION="70"

${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/getPharoVM.sh ${TEST_VM_VERSION} vm ${1}

IMAGE_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name Pharo*-${1}bit-*.image)
CHANGES_FILE=$(find . -name Pharo*-${1}bit-*.changes)
				
cp ${CACHE}/*.sources .
mv $IMAGE_FILE Pharo.image
mv $CHANGES_FILE Pharo.changes

export PHARO_CI_TESTING_ENVIRONMENT=1

./pharo Pharo.image test --junit-xml-output --stage-name=${2} '.*'
