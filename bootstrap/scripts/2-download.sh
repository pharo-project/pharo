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

if [ ! -e "${BOOTSTRAP_VMTARGET}" ]; then
    # Downloads a SPUR vm for the configured architecture
    mkdir ${BOOTSTRAP_CACHE}/vmtarget
    cd ${BOOTSTRAP_CACHE}/vmtarget
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/getPharoVM.sh 90 vm $BOOTSTRAP_ARCH
    cd -
    echo "Target VM: $(${VM} --version)"
fi

# Asuming if there is already a Pharo.image, we have the bootstrap
if [ ! -e "./Pharo.image" ]; then
	if [ ! -e "./bootstrapImage.zip" ]; then
		wget --progress=dot:mega https://github.com/guillep/PharoBootstrap/releases/download/v1.7.6/bootstrapImage.zip
	fi 
	unzip -u bootstrapImage.zip
	touch Pharo.image
fi

######################################################
## Cached resources

cd "${BOOTSTRAP_CACHE}"
pushd "${BOOTSTRAP_CACHE}"

if [ ! -e "./PharoV60.sources.zip" ]; then
	wget --progress=dot:mega http://files.pharo.org/sources/PharoV60.sources.zip
fi
unzip -u PharoV60.sources.zip

# update the commit hash as soon as you need a new version of the icons to be loaded
if [ ! -e "./icon-packs/idea11.zip" ]; then
	echo "Prepare icons"
	mkdir icon-packs
	cd icon-packs
	wget --progress=dot:mega https://github.com/pharo-project/pharo-icon-packs/archive/v1.0.2-idea11.zip -O idea11.zip
	cd -
fi

popd

