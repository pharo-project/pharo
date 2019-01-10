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
# WARNING: If you change this, you will need to change "runTests.sh" too
#
TEST_NAME_PREFIX=$(find ${CACHE} -name "Pharo*.zip" | head -n 1 | cut -d'/' -f 2 | cut -d'-' -f 1-2)
#TEST_VM_VERSION=$(echo "${TEST_NAME_PREFIX}" | cut -d'-' -f 1| cut -c 6- | cut -d'.' -f 1-2 | sed 's/\.//')
TEST_VM_VERSION="70"

${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/getPharoVM.sh ${TEST_VM_VERSION} vm ${1}
					
IMAGE_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-bootstrap-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name ${TEST_NAME_PREFIX}-bootstrap-${1}bit-*.image)

HERMES_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-hermesPackages-${1}bit-*.zip)
unzip $HERMES_ARCHIVE

RPACKAGE_ARCHIVE=$(find ${CACHE} -name ${TEST_NAME_PREFIX}-rpackage-${1}bit-*.zip)
unzip $RPACKAGE_ARCHIVE

mv $IMAGE_FILE bootstrap.image

export PHARO_CI_TESTING_ENVIRONMENT=1
	
#Initializing the Image
./pharo bootstrap.image
#Adding packages removed from the bootstrap
./pharo bootstrap.image loadHermes Hermes-Extensions.hermes --save
./pharo bootstrap.image loadHermes  Kernel-Chronology-Extras.hermes AST-Core.hermes Jobs.hermes InitializePackagesCommandLineHandler.hermes --save --no-fail-on-undeclared --on-duplication=ignore

#Initializing the package manager
./pharo bootstrap.image initializePackages --packages=packagesKernel.txt --protocols=protocolsKernel.txt --save

#Load traits
./pharo bootstrap.image loadHermes TraitsV2.hermes --save

#Loading Tests
./pharo bootstrap.image loadHermes SUnit-Core.hermes JenkinsTools-Core.hermes JenkinsTools-Core.hermes SUnit-Tests.hermes --save --no-fail-on-undeclared --on-duplication=ignore

#Running tests
./pharo bootstrap.image test --junit-xml-output --stage-name=${2} SUnit-Core SUnit-Tests
