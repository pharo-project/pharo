#!/bin/bash
#
# Restore the bootstrap snapshot.
#
# Only works if bootstrap-cache is in the default location
#
set -x
set -e

source bootstrap/scripts/envvars.sh

pushd "${REPOSITORY}"
bootstrap/scripts/1-clean.sh
tar xzf "${REPOSITORY}/snapshot.tar.gz"
popd
