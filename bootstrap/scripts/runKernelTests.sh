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

bootstrap/scripts/getPharoVM.sh 70 vm ${1}
					
IMAGE_ARCHIVE=$(find ${CACHE} -name Pharo7.0-bootstrap-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name Pharo7.0-bootstrap-${1}bit-*.image)

HERMES_ARCHIVE=$(find ${CACHE} -name Pharo7.0-hermesPackages-${1}bit-*.zip)
unzip $HERMES_ARCHIVE

RPACKAGE_ARCHIVE=$(find ${CACHE} -name Pharo7.0-rpackage-${1}bit-*.zip)
unzip $RPACKAGE_ARCHIVE

mv $IMAGE_FILE bootstrap.image

export PHARO_CI_TESTING_ENVIRONMENT=1
			
#Initializing the Image
./pharo bootstrap.image
#Adding packages removed from the bootstrap
./pharo bootstrap.image loadHermes Hermes-Extensions.hermes --save
./pharo bootstrap.image loadHermes Math-Operations-Extensions.hermes Debugging-Core.hermes Kernel-Chronology-Extras.hermes Collections-Atomic.hermes AST-Core.hermes Collections-Arithmetic.hermes Jobs.hermes InitializePackagesCommandLineHandler.hermes ReflectionMirrors-Primitives.hermes --save --no-fail-on-undeclared --on-duplication=ignore

#Initializing the package manager
./pharo bootstrap.image initializePackages --packages=packagesKernel.txt --protocols=protocolsKernel.txt --save

#Load traits
./pharo bootstrap.image loadHermes TraitsV2.hermes --save

#Loading Tests
./pharo bootstrap.image loadHermes SUnit-Core.hermes JenkinsTools-Core.hermes JenkinsTools-Core.hermes SUnit-Tests.hermes --save --no-fail-on-undeclared --on-duplication=ignore

#Running tests.
./pharo bootstrap.image test --junit-xml-output --stage-name=${2} SUnit-Core SUnit-Tests	
