#Get the hash of the built image
HASH=$(find Pharo7.0-32bit-*.zip | head -n 1 | cut -d '-' -f 3 | cut -d '.' -f 1)
FULL_IMAGE_NAME="Pharo7.0-32bit-${HASH}.zip"
MINIMAL_IMAGE_NAME="Pharo7.0-metacello-32bit-${HASH}.zip"
BUILD_NUMBER=${BUILD_NUMBER:-nobuildnumber}

cp "${FULL_IMAGE_NAME}" latest.zip
cp "${FULL_IMAGE_NAME}" latest-32.zip
cp "${MINIMAL_IMAGE_NAME}" latest-minimal.zip
cp "${MINIMAL_IMAGE_NAME}" latest-minimal-32.zip

for f in Pharo7.0-*.zip; do
	#If it is not base image
	if [[ "$f" != "FULL_IMAGE_NAME" ]]; then
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 2)
		mv "$f" Pharo${IMAGE_KIND}-7.0.0-arch.32bit.sha.${HASH}.build.${BUILD_NUMBER}.zip;
	fi
done