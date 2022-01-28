#!/bin/bash
#
# Remove any artifacts from previous bootstrap runs
#
set -x
set -e

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"

. ${SCRIPTS}/envvars.sh

rm -rf "${BOOTSTRAP_CACHE}"
