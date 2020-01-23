#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

ARCH=$1

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envversion.sh

#Get the hash of the built image

PHARO_NAME_PREFIX=$(find . -name "Pharo*.zip" | head -n 1 | cut -d'/' -f 2 | cut -d'-' -f 1-2)
HASH=$(find . -name "${PHARO_NAME_PREFIX}-${ARCH}bit*.zip" | head -n 1 | cut -d '-' -f 4 | cut -d'.' -f 1)
FULL_IMAGE_NAME="${PHARO_NAME_PREFIX}-${ARCH}bit-${HASH}.zip"

MINIMAL_IMAGE_NAME="${PHARO_NAME_PREFIX}-metacello-${ARCH}bit-${HASH}.zip"
BUILD_NUMBER=${BUILD_NUMBER:-nobuildnumber}

if [ $(ARCH) == 32]; then
	cp "${FULL_IMAGE_NAME}" latest.zip
	cp "${MINIMAL_IMAGE_NAME}" latest-minimal.zip
fi

cp "${FULL_IMAGE_NAME}" latest-${ARCH}.zip
cp "${MINIMAL_IMAGE_NAME}" latest-minimal-${ARCH}.zip

if [ $(is_release_build) == 1 ]; then
	cp "${FULL_IMAGE_NAME}" stable-${ARCH}.zip
fi

for f in ${PHARO_NAME_PREFIX}*-${ARCH}bit-*.zip; do
	#If it is not base image
	BITNESS=${ARCH}bit
	if [[ "$f" != "${FULL_IMAGE_NAME}" ]]; then
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 3)
		mv "$f" ${PHARO_NAME_PREFIX}-${IMAGE_KIND}.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	else
		mv "$f" ${PHARO_NAME_PREFIX}.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	fi
done
