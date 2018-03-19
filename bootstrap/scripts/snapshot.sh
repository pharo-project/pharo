#!/bin/bash
#
# Make a snapshot of the current bootstrap state.
#
# Only works if bootstrap-cache is in the default location
#
# The snapshot can then be restored with restore.sh
#
set -x
set -e

source bootstrap/scripts/envvars.sh

pushd "${REPOSITORY}"
tar czf "${REPOSITORY}/snapshot.tar.gz" Pharo.image Pharo.changes pharo pharo-ui pharo-vm pharo-local vmtarget bootstrap-cache
popd
