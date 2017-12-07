set -x
set -e

wget -O - get.pharo.org/vm60 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.2/bootstrapImage.zip
unzip bootstrapImage.zip

CACHE="${BOOTSTRAP_CACHE:-bootstrap-cache}"
REPOSITORY="${BOOTSTRAP_REPOSITORY:-.}"

./pharo Pharo.image ${REPOSITORY}/bootstrap/scripts/prepare_image.st --save --quit
mkdir -p "${CACHE}" #required to generate hermes files
./pharo Pharo.image ${REPOSITORY}/bootstrap/scripts/generateKernelHermesFiles.st --quit
./pharo Pharo.image ${REPOSITORY}/bootstrap/scripts/generateCompilerHermesFiles.st --quit
./pharo Pharo.image ${REPOSITORY}/bootstrap/scripts/generateSUnitHermesFiles.st --quit
./pharo Pharo.image ${REPOSITORY}/bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --BUILD_NUMBER=${BUILD_NUMBER} --quit

bash ${REPOSITORY}/bootstrap/scripts/build.sh
