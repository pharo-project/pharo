#!/usr/bin/env bash

set -ex

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envversion.sh

# I will use the name of the image to determine the vm version (because file name is in the format Pharo7.0.0-rc1)
#
PHARO_NAME_PREFIX=$(find . -name "Pharo*-bootstrap*.zip" | head -n 1 | cut -d'/' -f 2 | cut -d'-' -f 1-2)
PHARO_SHORT_VERSION=$(git describe --long --tags --first-parent | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//')

PACKAGED_VERSIONS="latest*.zip"
if [ $(is_release_build) == 1 ]; then
	PACKAGED_VERSIONS="${PACKAGED_VERSIONS} stable*.zip"
fi

destDir="/appli/files.pharo.org/image/${PHARO_SHORT_VERSION}/"
echo "Uploading Images to pharo.files.org/$destDir"

scp -o StrictHostKeyChecking=no -v \
  ${PACKAGED_VERSIONS} \
  ${PHARO_NAME_PREFIX}.build.*.zip \
  ${PHARO_NAME_PREFIX}-metacello.build.*.zip \
    pharo-ci@files.pharo.org:image/${PHARO_SHORT_VERSION}/
