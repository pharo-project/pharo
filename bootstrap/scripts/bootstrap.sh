wget -O - get.pharo.org/vm60 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.1.1/bootstrapImage.zip
unzip bootstrapImage.zip

./pharo Pharo.image ./bootstrap/scripts/prepare_image.st --save --quit
mkdir -p bootstrap-cache #required to generate hermes files
./pharo Pharo.image ./bootstrap/scripts/generateHermesFiles.st --quit
./pharo Pharo.image ./bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --quit

bash ./bootstrap/scripts/build.sh