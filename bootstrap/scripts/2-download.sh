#!/usr/bin/env bash
#
# Download resources required for bootstrap process
#
# See envvars.sh for input environment variables
#
set -x
set -e

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"

. ${SCRIPTS}/envvars.sh

mkdir -p "${BOOTSTRAP_CACHE}" #required to generate hermes files

${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/getPharoVM.sh 70
wget --progress=dot:mega https://github.com/carolahp/PharoBootstrap/releases/download/v1.7.0/bootstrapImage.zip
unzip bootstrapImage.zip

cd "${BOOTSTRAP_CACHE}"
#We need the old sources file next to the image because of sources condensation step
wget --progress=dot:mega http://files.pharo.org/sources/PharoV60.sources
echo "Prepare icons"
mkdir icon-packs
cd icon-packs
# update the commit hash as soon as you need a new version of the icons to be loaded
wget --progress=dot:mega https://github.com/pharo-project/pharo-icon-packs/archive/v1.0.0-idea11.zip -O idea11.zip
cd ..
cd ..

if [ -z "${BOOTSTRAP_VMTARGET}" ]
then
    # Downloads a SPUR vm for the configured architecture
    mkdir ${BOOTSTRAP_CACHE}/vmtarget
    cd ${BOOTSTRAP_CACHE}/vmtarget
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/getPharoVM.sh 70 vm $BOOTSTRAP_ARCH
    cd -
fi
echo "Target VM: $(${VM} --version | grep Hash)"

