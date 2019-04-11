#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envvars.sh

set_version_variables

#Load VMMaker, used to convert images from 32 to 64 bits
mkdir -p vmmaker && cd vmmaker
wget https://github.com/pharo-project/pharo-32to64-converter/releases/download/v1.0.0/vmmaker-image.zip
unzip vmmaker-image.zip
../../bootstrap/scripts/getPharoVM.sh 60
wget http://files.pharo.org/sources/PharoV50.sources
cd ..

#We convert all files that are not full image (Pharo7.0-${IMAGE_KIND}-32bit-${HASH}.*)
for f in ${PHARO_NAME_PREFIX}-*-32bit-*.zip; do
	unzip "$f"
	IMAGEFILENAME=$(find . -iname *.image -maxdepth 1)
	IMAGENAME=${IMAGEFILENAME%.*}
	
	if [ -f "${IMAGENAME}.image" ]
	then
		mv "${IMAGENAME}.image" tempconversion.image
		touch "${IMAGENAME}.changes"
		mv "${IMAGENAME}.changes" tempconversion.changes
	
		IMAGE_KIND=$(echo "$f" | cut -d '-' -f 3)
		HASH=$(echo "$f" | head -n 1 | cut -d '-' -f 5 | cut -d '.' -f 1)
		./vmmaker/pharo ./vmmaker/generator.image eval "[Spur32to64BitBootstrap new bootstrapImage: '../tempconversion.image'] on: AssertionFailure do: [ :fail | fail resumeUnchecked: nil ]"

		mv "tempconversion-64.image" "${PHARO_NAME_PREFIX}-${IMAGE_KIND}-64bit-$HASH.image"
		mv "tempconversion-64.changes" "${PHARO_NAME_PREFIX}-${IMAGE_KIND}-64bit-$HASH.changes"
		zip ${PHARO_NAME_PREFIX}-${IMAGE_KIND}-64bit-$HASH.zip ${PHARO_NAME_PREFIX}-${IMAGE_KIND}-64bit-$HASH.*
	fi
	rm -f *.image *.changes *.sources
done

#We convert full image file (Pharo7.0-32bit-${HASH}.*)
for f in  ${PHARO_NAME_PREFIX}-32bit-*.zip; do
	unzip "$f"
	IMAGENAME=${f%.*}
	cp "${IMAGENAME}.image" tempconversion.image
	touch tempconversion.changes
	cp "${IMAGENAME}.changes" tempconversion.changes
	
	HASH=$(find $f | head -n 1 | cut -d '-' -f 4 | cut -d '.' -f 1)
	./vmmaker/pharo ./vmmaker/generator.image eval "[Spur32to64BitBootstrap new bootstrapImage: '../tempconversion.image'] on: AssertionFailure do: [ :fail | fail resumeUnchecked: nil ]"
	
	mv "tempconversion-64.image" "${PHARO_NAME_PREFIX}-64bit-$HASH.image"
	mv "tempconversion-64.changes" "${PHARO_NAME_PREFIX}-64bit-$HASH.changes"
	
	# fix the display size in the image header (position 40 [zero based], 24 for 32-bit image)
	# in older versions we must use octal representation
	printf "\231\002\320\003" > displaySize.bin
	dd if="displaySize.bin" of="${PHARO_NAME_PREFIX}-64bit-$HASH.image" bs=1 seek=40 count=4 conv=notrunc
	
	PHARO_SOURCES_PREFIX=$(echo "${PHARO_NAME_PREFIX}" | cut -d'-' -f 1 | cut -d'.' -f 1-2)
	zip ${PHARO_NAME_PREFIX}-64bit-$HASH.zip ${PHARO_NAME_PREFIX}-64bit-$HASH.* ${PHARO_SOURCES_PREFIX}*.sources pharo.version
	rm -f *.image *.changes *.sources
done

rm -rf vmmaker
