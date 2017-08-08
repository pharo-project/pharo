set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

#Get the hash of the built image
HASH=$(find Pharo7.0-32bit-*.zip | head -n 1 | cut -d '-' -f 3 | cut -d '.' -f 1)
FULL_IMAGE_NAME32="Pharo7.0-32bit-${HASH}.zip"
FULL_IMAGE_NAME64="Pharo7.0-64bit-${HASH}.zip"

MINIMAL_IMAGE_NAME32="Pharo7.0-metacello-32bit-${HASH}.zip"
MINIMAL_IMAGE_NAME64="Pharo7.0-metacello-64bit-${HASH}.zip"
BUILD_NUMBER=${BUILD_NUMBER:-nobuildnumber}

cp "${FULL_IMAGE_NAME32}" latest.zip
cp "${FULL_IMAGE_NAME32}" latest-32.zip
cp "${FULL_IMAGE_NAME64}" latest-64.zip
cp "${MINIMAL_IMAGE_NAME32}" latest-minimal.zip
cp "${MINIMAL_IMAGE_NAME32}" latest-minimal-32.zip
cp "${MINIMAL_IMAGE_NAME64}" latest-minimal-64.zip

for f in Pharo7.0*-32bit-*.zip; do
	#If it is not base image
	BITNESS=32bit
	if [[ "$f" != "${FULL_IMAGE_NAME32}" ]]; then
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 2)
		mv "$f" Pharo${IMAGE_KIND}-7.0.0-alpha.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	else
		mv "$f" Pharo-7.0.0-alpha.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	fi
done

for f in Pharo7.0*-64bit-*.zip; do
	#If it is not base image
	BITNESS=64bit
	echo $f
	echo ${FULL_IMAGE_NAME64}
	if [[ "$f" != "${FULL_IMAGE_NAME64}" ]]; then
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 2)
		mv "$f" Pharo${IMAGE_KIND}-7.0.0-alpha.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	else
		mv "$f" Pharo-7.0.0-alpha.build.${BUILD_NUMBER}.sha.${HASH}.arch.${BITNESS}.zip;
	fi
done
