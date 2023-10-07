#!/bin/bash
#
# Remove any artifacts from previous bootstrap runs
#
set -x
set -e

echo $(date -u) "Bootstrap: Begin cleanup of artifacts of previous runs"

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"

. ${SCRIPTS}/envvars.sh

rm -rf "${BOOTSTRAP_CACHE}"
