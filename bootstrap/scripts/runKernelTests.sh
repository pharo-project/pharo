#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

#
# This script loads packages for a minimal image and run tests on it to ensure it is working fine!
#

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

# The first parameter is the architecture
# The second parameter is the stage name

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envvars.sh

CACHE="${BOOTSTRAP_CACHE}"


find ${CACHE}

# I will use the name of the image to determine the vm version (because file name is in the format Pharo7.0.0-rc1)
#
# WARNING: I'm assuming CACHE=bootstrap-cache
# WARNING: If you change this, you will need to change "runTests.sh" too
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

${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/getPharoVM.sh ${TEST_VM_VERSION} ${TEST_VM_KIND} ${1}
					
IMAGE_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-bootstrap-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name ${TEST_NAME_PREFIX}-bootstrap-${1}bit-*.image)
HERMES_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-hermesPackages-${1}bit-*.zip)
unzip $HERMES_ARCHIVE

mv $IMAGE_FILE bootstrap.image

export PHARO_CI_TESTING_ENVIRONMENT=1

#Adding packages removed from the bootstrap
./pharo bootstrap.image perform --save BasicHermesTool load: --as-array $(cat hermesSUnitPackages.txt)
./pharo bootstrap.image perform --save Pragma buildCache

#Running tests. We should also run Kernel-Tests but it is currently failing
./pharo bootstrap.image test --junit-xml-output --stage-name ${2} SUnit-Tests 
