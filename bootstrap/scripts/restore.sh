#!/usr/bin/env bash
#
# Restore the bootstrap snapshot.
#
# This script must be run under the same conditions as snapshot.sh.
# Please read the comments in snapshot.sh.
#
set -x
set -e

. ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/envvars.sh

${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/1-clean.sh
tar xzf snapshot.tar.gz
