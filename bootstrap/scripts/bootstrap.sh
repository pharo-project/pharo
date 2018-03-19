set -x
set -e

wget -O - get.pharo.org/vm61 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.4/bootstrapImage.zip
unzip bootstrapImage.zip

CACHE="${BOOTSTRAP_CACHE:-bootstrap-cache}"
REPOSITORY="${BOOTSTRAP_REPOSITORY:-.}"

./pharo Pharo.image ${REPOSITORY}/bootstrap/scripts/prepare_image.st --save --quit
mkdir -p "${CACHE}" #required to generate hermes files
./pharo Pharo.image eval "PBBootstrap fromCommandLine bootstrap" --quit

bash ${REPOSITORY}/bootstrap/scripts/build.sh
