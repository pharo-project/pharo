#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envversion.sh

# The first parameter is the architecture
# The second parameter is the stage name

CACHE="${BOOTSTRAP_CACHE:-bootstrap-cache}"

find ${CACHE}

${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/getPharoVM.sh ${PHARO_VM_VERSION} vm ${1}
					
IMAGE_ARCHIVE=$(find ${CACHE} -name ${PHARO_NAME_PREFIX}-${1}bit-*.zip)
unzip $IMAGE_ARCHIVE
IMAGE_FILE=$(find . -name ${PHARO_NAME_PREFIX}-${1}bit-*.image)
CHANGES_FILE=$(find . -name ${PHARO_NAME_PREFIX}-${1}bit-*.changes)
				
cp ${CACHE}/*.sources .
mv $IMAGE_FILE Pharo.image
mv $CHANGES_FILE Pharo.changes

export PHARO_CI_TESTING_ENVIRONMENT=1

./pharo Pharo.image test --junit-xml-output --stage-name=${2} '.*'
