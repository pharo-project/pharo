#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io
set -o nounset
set -o xtrace

#Load VMMaker, used to convert images from 32 to 64 bits
mkdir -p vmmaker && cd vmmaker
wget https://ci.inria.fr/pharo/view/5.0-VM-Spur/job/Spur-Git-Tracker/lastSuccessfulBuild/artifact/vmmaker-image.zip
unzip vmmaker-image.zip
wget -O - http://get.pharo.org/vm60 | bash
wget http://files.pharo.org/sources/PharoV50.sources
cd ..

#We convert all files that are not full image (Pharo7.0-${IMAGE_KIND}-32bit-${HASH}.*)
for f in Pharo7.0-*-32bit-*.zip; do
	unzip "$f"
	IMAGENAME=${f%.*}
	mv "${IMAGENAME}.image" tempconversion.image
	touch "${IMAGENAME}.changes"
	mv "${IMAGENAME}.changes" tempconversion.changes
	
	IMAGE_KIND=$(echo "${IMAGENAME}" | cut -d '-' -f 2)
	HASH=$(echo "$f" | head -n 1 | cut -d '-' -f 4 | cut -d '.' -f 1)
	./vmmaker/pharo ./vmmaker/generator.image eval "[Spur32to64BitBootstrap new bootstrapImage: '../tempconversion.image'] on: AssertionFailure do: [ :fail | fail resumeUnchecked: nil ]"

	mv "tempconversion-64.image" "Pharo7.0-${IMAGE_KIND}-64bit-$HASH.image"
	mv "tempconversion-64.changes" "Pharo7.0-${IMAGE_KIND}-64bit-$HASH.changes"
	zip Pharo7.0-${IMAGE_KIND}-64bit-$HASH.zip Pharo7.0-${IMAGE_KIND}-64bit-$HASH.*
	rm *.image *.changes *.sources
done

#We convert full image file (Pharo7.0-32bit-${HASH}.*)
for f in Pharo7.0-32bit-*.zip; do
	unzip "$f"
	IMAGENAME=${f%.*}
	cp "${IMAGENAME}.image" tempconversion.image
	touch tempconversion.changes
	cp "${IMAGENAME}.changes" tempconversion.changes
	
	HASH=$(find $f | head -n 1 | cut -d '-' -f 3 | cut -d '.' -f 1)
	./vmmaker/pharo ./vmmaker/generator.image eval "[Spur32to64BitBootstrap new bootstrapImage: '../tempconversion.image'] on: AssertionFailure do: [ :fail | fail resumeUnchecked: nil ]"
	
	mv "tempconversion-64.image" "Pharo7.0-64bit-$HASH.image"
	mv "tempconversion-64.changes" "Pharo7.0-64bit-$HASH.changes"
	zip Pharo7.0-64bit-$HASH.zip Pharo7.0-64bit-$HASH.* ${IMAGENAME}.sources
	rm *.image *.changes *.sources
done

rm -rf vmmaker
