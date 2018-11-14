#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

#Get the hash of the built image

PHARO_NAME_PREFIX=$(find . -name "Pharo*.zip" | head -n 1 | cut -d'/' -f 2 | cut -d'-' -f 1-2)
HASH=$(find . -name "${PHARO_NAME_PREFIX}-32bit*.zip" | head -n 1 | cut -d '-' -f 4 | cut -d'.' -f 1)
FULL_IMAGE_NAME32="${PHARO_NAME_PREFIX}-32bit-${HASH}.zip"
FULL_IMAGE_NAME64="${PHARO_NAME_PREFIX}-64bit-${HASH}.zip"

MINIMAL_IMAGE_NAME32="${PHARO_NAME_PREFIX}-metacello-32bit-${HASH}.zip"
MINIMAL_IMAGE_NAME64="${PHARO_NAME_PREFIX}-metacello-64bit-${HASH}.zip"
BUILD_NUMBER=${BUILD_NUMBER:-nobuildnumber}

cp "${FULL_IMAGE_NAME32}" latest.zip
cp "${FULL_IMAGE_NAME32}" latest-32.zip
cp "${FULL_IMAGE_NAME64}" latest-64.zip
cp "${MINIMAL_IMAGE_NAME32}" latest-minimal.zip
cp "${MINIMAL_IMAGE_NAME32}" latest-minimal-32.zip
cp "${MINIMAL_IMAGE_NAME64}" latest-minimal-64.zip

for f in ${PHARO_NAME_PREFIX}*-32bit-*.zip; do
	#If it is not base image
	BITNESS=32bit
	if [[ "$f" != "${FULL_IMAGE_NAME32}" ]]; then
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 3)
		mv "$f" ${PHARO_NAME_PREFIX}-${IMAGE_KIND}.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	else
		mv "$f" ${PHARO_NAME_PREFIX}.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	fi
done

for f in ${PHARO_NAME_PREFIX}*-64bit-*.zip; do
	#If it is not base image
	BITNESS=64bit
	echo $f
	echo ${FULL_IMAGE_NAME64}
	if [[ "$f" != "${FULL_IMAGE_NAME64}" ]]; then
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 3)
		mv "$f" ${PHARO_NAME_PREFIX}-${IMAGE_KIND}.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	else
		mv "$f" ${PHARO_NAME_PREFIX}.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	fi
done
