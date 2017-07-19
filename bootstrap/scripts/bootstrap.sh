wget -O - get.pharo.org/vm60 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.1/bootstrapImage.zip
unzip bootstrapImage.zip

./pharo Pharo.image ./bootstrap/scripts/prepare_image.st --save --quit
./pharo Pharo.image ./bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --quit
#bash ./bootstrap/scripts/build.sh