#!/bin/bash
#
# Download resources required for bootstrap process
#
set -x
set -e

source bootstrap/scripts/envvars.sh

mkdir -p "${CACHE}" #required to generate hermes files

wget -O - get.pharo.org/vm61 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.4.1/bootstrapImage.zip
unzip bootstrapImage.zip

pushd "${CACHE}"
#We need the old sources file next to the image because of sources condensation step
wget http://files.pharo.org/sources/PharoV60.sources
echo "Prepare icons"
mkdir icon-packs
pushd icon-packs
# update the commit hash as soon as you need a new version of the icons to be loaded
wget http://github.com/pharo-project/pharo-icon-packs/archive/57fba57a02ef3b96c453fb9feba7b71c6a3e618e.zip -O idea11.zip
popd
popd


# Downloads a SPUR vm for the configured architecture
mkdir vmtarget
pushd vmtarget
if [ ${BOOTSTRAP_ARCH} = "64" ]; then
	ARCHFLAG=64/
fi
wget -O- get.pharo.org/${ARCHFLAG}vm70 | bash
popd

