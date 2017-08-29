#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

ARCHFLAG=""
if [ ${1} = "64" ]; then
    ARCHFLAG="64/"
fi

find bootstrap-cache

wget -O- get.pharo.org/${ARCHFLAG}vm70 | bash
					
IMAGE_ARCHIVE=$(find bootstrap-cache -name Pharo7.0-bootstrap-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name Pharo7.0-bootstrap-${1}bit-*.image)

HERMES_ARCHIVE=$(find bootstrap-cache -name Pharo7.0-hermesPackages-${1}bit-*.zip)
unzip $HERMES_ARCHIVE

RPACKAGE_ARCHIVE=$(find bootstrap-cache -name Pharo7.0-rpackage-${1}bit-*.zip)
unzip $RPACKAGE_ARCHIVE

mv $IMAGE_FILE bootstrap.image
			
./pharo bootstrap.image
./pharo bootstrap.image initializePackages --packages=packagesKernel.txt --protocols=protocolsKernel.txt --save
./pharo bootstrap.image loadHermes SUnit-Core.hermes JenkinsTools-Core.hermes --save
./pharo bootstrap.image test --junit-xml-output SUnit-Core			
