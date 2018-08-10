#!/usr/bin/env bash
#
# Make a snapshot of the current bootstrap state.
#
# This script must be run under the same conditions as bootstrap.sh, i.e.:
#
# - the current working directory the same
# - BOOTSTRAP_REPOSITORY and BOOTSTRAP_CACHE are defined with the same values
#
# The snapshot can then be restored with restore.sh
#
set -x
set -e

. ${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/envvars.sh

tar czf snapshot.tar.gz bootstrapImage.zip Pharo.image Pharo.changes pharo pharo-ui pharo-vm pharo-local ${BOOTSTRAP_CACHE}
