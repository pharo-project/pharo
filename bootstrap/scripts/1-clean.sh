#!/bin/bash
#
# Remove any artifacts from previous bootstrap runs
#
set -x
set -e

source bootstrap/scripts/envvars.sh

pushd "${REPOSITORY}"
rm -f bootstrapImage.zip
rm -f Pharo.image Pharo.changes pharo pharo-ui
rm -rf pharo-vm
rm -rf pharo-local
rm -rf vmtarget
rm -rf "${CACHE}"
popd
