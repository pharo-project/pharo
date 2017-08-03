if [ ${1} = "64" ]; then
	ARCHFLAG=64/
fi

wget -O- get.pharo.org/${ARCHFLAG}vm70 | bash
					
IMAGE_ARCHIVE = `find bootstrap-cache -name 'Pharo7.0-${architecture}bit-*.zip'`

unzip $IMAGE_ARCHIVE
IMAGE_FILE=`find . -name 'Pharo7.0-${architecture}bit-*.image'`
CHANGES_FILE=`find . -name 'Pharo7.0-${architecture}bit-*.changes'`
				
cp bootstrap-cache/*.sources .
mv $IMAGE_FILE Pharo.image
mv $CHANGES_FILE Pharo.changes
					
./pharo Pharo.image test --junit-xml-output \".*\"