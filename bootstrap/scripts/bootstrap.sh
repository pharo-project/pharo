wget -O - get.pharo.org/${PHARO_VERSION}+vm | bash
./pharo Pharo.image ./bootstrap/scripts/prepare_image.st --save --quit

./pharo Pharo.image ./bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --quit
bash ./bootstrap/scripts/build.sh