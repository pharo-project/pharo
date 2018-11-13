#!/usr/bin/env bash

set -ex

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envversion.sh

destDir="/appli/files.pharo.org/image/${PHARO_SHORT_VERSION}/"
echo "Uploading Images to pharo.files.org/$destDir"
scp -o StrictHostKeyChecking=no -v \
  latest*.zip \
  #{PHARO_NAME_PREFIX}*.zip \
  Pharo-metacello*.zip \
    pharoorgde@ssh.cluster023.hosting.ovh.net:files/image/${PHARO_SHORT_VERSION}/

# this is not being used
#scp -o StrictHostKeyChecking=no -v \
#  #{PHARO_NAME_PREFIX}-bootstrap*.zip \
#  #{PHARO_NAME_PREFIX}-core*.zip \
#  #{PHARO_NAME_PREFIX}-compiler*.zip \
#  #{PHARO_NAME_PREFIX}-monticello*.zip \
#  #{PHARO_NAME_PREFIX}-rpackage*.zip \
#  #{PHARO_NAME_PREFIX}-hermesPackages*.zip \
#    pharoorgde@ssh.cluster023.hosting.ovh.net:files/image/${PHARO_SHORT_VERSION}/bootstrap/
