#!/usr/bin/env bash
#
# Set up Pharo based versions. 
# 
# - current image version prefix, e.g. "Pharo7.0.0-rc1"
# - vm version for the current image, to be used to download the correct vm, e.g. "70"


PHARO_NAME_PREFIX="Pharo`git describe --long --tags | cut -d'-' -f 1-2 | cut -c 2-`"
PHARO_SHORT_VERSION=`git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//'`
# this is just to make things clear (and because they could change in the future, who knows :P)
PHARO_VM_VERSION="${PHARO_SHORT_VERSION}"