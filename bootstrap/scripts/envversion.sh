#!/usr/bin/env bash
#
# Set up Pharo based versions. 
# 
# - current image version prefix, e.g. "Pharo7.0.0-rc1"
# - vm version for the current image, to be used to download the correct vm, e.g. "70"

# Versions are taken from tags. Tag should have a format like vMAJOR.MINOR.PATCH[-EXTRA], e.g. v7.0.0-rc1
PHARO_VERSION_PREFIX="Pharo$(git describe --long --tags | cut -d'-' -f 1-2 | cut -c 2-)"
# Snapshots are taken from branchs. Branchs should be named dev-MAJOR.MINOR (example: dev-7.0)
PHARO_SNAPSHOT_PREFIX="Pharo$(git branch | grep \* | cut -d' ' -f 2 | cut -d'-' -f 2)-SNAPSHOT"

PHARO_SHORT_VERSION=$(git branch | grep \* | cut -d' ' -f 2 | cut -d'-' -f 2 | sed 's/\.//')
# this is just to make things clear (and because they could change in the future, who knows :P)
PHARO_VM_VERSION="${PHARO_SHORT_VERSION}"

# use this to know if we are in a version or a SNAPSHOT
function is_version() {
	set -f
	local versionTag=$(git tag --list --points-at HEAD | grep -E "^v[0-9]+\.[0-9]+.[0-9]+(\-[a-zA-Z0-9_]+)?$")
    if [ "${versionTag}" == "" ]; then
        echo 0
    else
        echo 1
    fi
}
