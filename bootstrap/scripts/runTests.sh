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

CACHE="${BOOTSTRAP_CACHE:-bootstrap-cache}"

find ${CACHE}

wget -O- get.pharo.org/${ARCHFLAG}vm70 | bash
					
IMAGE_ARCHIVE=$(find ${CACHE} -name Pharo7.0-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name Pharo7.0-${1}bit-*.image)
CHANGES_FILE=$(find . -name Pharo7.0-${1}bit-*.changes)
				
cp ${CACHE}/*.sources .
mv $IMAGE_FILE Pharo.image
mv $CHANGES_FILE Pharo.changes

export PHARO_CI_TESTING_ENVIRONMENT=1
					
./pharo Pharo.image test --junit-xml-output '.*'
