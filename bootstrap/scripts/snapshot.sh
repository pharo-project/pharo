#!/usr/bin/env bash
#
# Make a snapshot of the current bootstrap state.
#
# Only works if bootstrap-cache is in the default location
#
# The snapshot can then be restored with restore.sh
#
set -x
set -e

. ${BOOTSTRAP_REPOSITORY:-.}/bootstrap/scripts/envvars.sh

pushd "${BOOTSTRAP_REPOSITORY}"
tar czf "${BOOTSTRAP_REPOSITORY}/snapshot.tar.gz" Pharo.image Pharo.changes pharo pharo-ui pharo-vm pharo-local ${CACHE}
popd
