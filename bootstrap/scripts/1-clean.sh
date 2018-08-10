#!/bin/bash
#
# Remove any artifacts from previous bootstrap runs
#
set -x
set -e

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"

. ${SCRIPTS}/envvars.sh

rm -f bootstrapImage.zip
rm -f Pharo.image Pharo.changes pharo pharo-ui
rm -rf pharo-vm
rm -rf pharo-local
rm -rf "${BOOTSTRAP_CACHE}"
