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

# Since there is random failure during tests execution we print the content of the current directory to find potential problems
bootstrap/scripts/printFolderContent.sh

bootstrap/scripts/getPharoVM.sh 70 vm ${1}
					
IMAGE_ARCHIVE=$(find ${CACHE} -name Pharo7.0-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name Pharo7.0-${1}bit-*.image)
CHANGES_FILE=$(find . -name Pharo7.0-${1}bit-*.changes)
				
cp ${CACHE}/*.sources .
mv $IMAGE_FILE Pharo.image
mv $CHANGES_FILE Pharo.changes

export PHARO_CI_TESTING_ENVIRONMENT=1
					
./pharo Pharo.image test --junit-xml-output --stage-name=${2} '.*'
