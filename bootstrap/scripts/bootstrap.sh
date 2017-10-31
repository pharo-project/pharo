set -x
set -e

wget -O - get.pharo.org/vm60 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.2/bootstrapImage.zip
unzip bootstrapImage.zip

./pharo Pharo.image ./bootstrap/scripts/prepare_image.st --save --quit
mkdir -p bootstrap-cache #required to generate hermes files
./pharo Pharo.image ./bootstrap/scripts/generateKernelHermesFiles.st --quit
./pharo Pharo.image ./bootstrap/scripts/generateSUnitHermesFiles.st --quit
./pharo Pharo.image ./bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --BUILD_NUMBER=${BUILD_NUMBER} --quit

bash ./bootstrap/scripts/build.sh