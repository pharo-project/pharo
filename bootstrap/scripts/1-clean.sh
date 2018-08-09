#!/bin/bash
#
# Remove any artifacts from previous bootstrap runs
#
set -x
set -e

. ${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/envvars.sh

rm -f bootstrapImage.zip
rm -f Pharo.image Pharo.changes pharo pharo-ui
rm -rf pharo-vm
rm -rf pharo-local
rm -rf "${BOOTSTRAP_CACHE}"
