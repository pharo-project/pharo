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
mkdir -p "${BOOTSTRAP_DOWNLOADS}"

##
# Download section
##

function download_to {
	wget --progress=dot:mega "$1" -O "$2"
}

if [ ! -e "${BOOTSTRAP_VMTARGET}" ]; then
    # Downloads a SPUR vm for the configured architecture
    mkdir ${BOOTSTRAP_DOWNLOADS}/vmtarget
    cd ${BOOTSTRAP_DOWNLOADS}/vmtarget
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/getPharoVM.sh 90 vm $BOOTSTRAP_ARCH
    cd -
    echo "Target VM: $(${VM} --version)"
fi

if [ ! -e "${BOOTSTRAP_DOWNLOADS}/bootstrapImage.zip" ]; then
	download_to https://github.com/guillep/PharoBootstrap/releases/download/v1.7.6/bootstrapImage.zip ${BOOTSTRAP_DOWNLOADS}/bootstrapImage.zip
fi 

# checking for PharoV60.sources
if [ ! -e "${BOOTSTRAP_DOWNLOADS}/PharoV60.sources.zip" ]; then
	download_to http://files.pharo.org/sources/PharoV60.sources.zip ${BOOTSTRAP_DOWNLOADS}/PharoV60.sources.zip
fi

# checking for icons
# update the commit hash as soon as you need a new version of the icons to be loaded
if [ ! -e "${BOOTSTRAP_DOWNLOADS}/idea11.zip" ]; then
	download_to https://github.com/pharo-project/pharo-icon-packs/archive/v1.0.2-idea11.zip ${BOOTSTRAP_DOWNLOADS}/idea11.zip
fi

##
# Preparation section (unziping/copying files to appropriate place)
##

# bootstrap image
if [ ! -e "./Pharo.image" ]; then
	unzip -u ${BOOTSTRAP_DOWNLOADS}/bootstrapImage.zip -d .
fi

# PharoV6 sources
if [ ! -e "${BOOTSTRAP_CACHE}/PharoV60.sources" ]; then
	unzip -u ${BOOTSTRAP_DOWNLOADS}/PharoV60.sources.zip -d ${BOOTSTRAP_CACHE}
fi

# Icons
if [ ! -e ${BOOTSTRAP_CACHE}/icon-packs/idea11.zip ]; then
	mkdir -p ${BOOTSTRAP_CACHE}/icon-packs
	cp ${BOOTSTRAP_DOWNLOADS}/idea11.zip ${BOOTSTRAP_CACHE}/icon-packs/idea11.zip
fi


