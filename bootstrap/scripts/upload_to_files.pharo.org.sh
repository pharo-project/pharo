#!/usr/bin/env bash

set -ex

# I will use the name of the image to determine the vm version (because file name is in the format Pharo7.0.0-rc1)
#
PHARO_NAME_PREFIX=$(find . -name "Pharo*-bootstrap*.zip" | head -n 1 | cut -d'/' -f 2 | cut -d'-' -f 1-2)
PHARO_SHORT_VERSION=$(echo "${PHARO_NAME_PREFIX}" | cut -d'-' -f 1 | cut -c 6- | cut -d'.' -f 1-2 | sed 's/\.//')

destDir="/appli/files.pharo.org/image/${PHARO_SHORT_VERSION}/"
echo "Uploading Images to pharo.files.org/$destDir"

scp -o StrictHostKeyChecking=no -v \
  latest*.zip \
  ${PHARO_NAME_PREFIX}.build.*.zip \
  ${PHARO_NAME_PREFIX}-metacello.build.*.zip \
    pharoorgde@ssh.cluster023.hosting.ovh.net:files/image/${PHARO_SHORT_VERSION}/
