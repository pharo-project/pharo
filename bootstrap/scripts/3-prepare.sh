#!/usr/bin/env bash
#
# Prepare the image used for bootstrapping
#
set -x
set -e

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"

. ${SCRIPTS}/envvars.sh

./pharo Pharo.image ${IMAGE_FLAGS} ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/prepare_image.st --save --quit
./pharo Pharo.image ${IMAGE_FLAGS} ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --BUILD_NUMBER=${BUILD_NUMBER} --VERSION_INFO=`GIT_DIR=${BOOTSTRAP_REPOSITORY}/.git git describe --long --tags --abbrev=40` --quit
