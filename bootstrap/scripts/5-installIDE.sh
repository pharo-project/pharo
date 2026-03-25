#!/bin/bash
#
# Bootstrap the new image
#
set -x
set -e

echo $(date -u) "Bootstrap: Beginning to build the new image"

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envvars.sh

set_version_variables

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

function show_help {
  echo
  "Pharo Build Script
  ==================
  This script assumes the existence of a metacello image. Then it proceeds to
    - Load the rest of the image using BaselineOfIDE
    - output: Pharo.image and changes file

  Script arguments
  ================
  -a
    Architecture of the image. Indicates wether the image will be a 32 bit or 64 bit artifact.
    Expected values: 32, 64
  -h -?
    Prints this help message
  "
  exit 0
}

# Initialize our own variables:
ARCH_DESCRIPTION=${BOOTSTRAP_ARCH}

# Use -gt 1 to consume two arguments per pass in the loop (e.g. each
# argument has a corresponding value to go with it).
# Use -gt 0 to consume one or more arguments per pass in the loop (e.g.
# some arguments don't have a corresponding value to go with it such
# as in the --default example).
# note: if this is set to -gt 0 the /etc/hosts part is not recognized ( may be a bug )
while getopts "h?a:d" opt; do
    case "${opt}" in
    a)
        if [[ "${OPTARG}" -ne "32"  &&  "${OPTARG}" -ne "64" ]]; then
          echo "Invalid Option ${OPTARG}: expected architecture values are 32 or 64";
          exit 1;
        fi
        ARCH_DESCRIPTION=${OPTARG};
        ;;
    d)
        DESCRIBE=1;
        ;;
    h|\?)
        show_help;
        exit 0;
        ;;
  esac
done

shift $((OPTIND-1))
[ "$1" = "--" ] && shift

if [ -z "${ARCH_DESCRIPTION}" ]; then
  echo "No architecture specified. Please set the BOOTSTRAP_ARCH environment variable or use the -a argument";
  exit 1;
fi


if [[ -z "${BOOTSTRAP_REPOSITORY}" ]]; then
  GIT_COMMIT_HASH=$(git show -s --format=%h)
else
  GIT_COMMIT_HASH=$(git -C ${BOOTSTRAP_REPOSITORY} show -s --format=%h)
fi

SUFFIX=${ARCH_DESCRIPTION}bit-${GIT_COMMIT_HASH}

if [[ ${DESCRIBE} -eq "1" ]]; then
  echo "${SUFFIX}"
  exit 0
fi

METACELLO_IMAGE_NAME=${PHARO_NAME_PREFIX}-metacello-${SUFFIX}
PHARO_IMAGE_NAME=${PHARO_NAME_PREFIX}-${SUFFIX}

#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd "${BOOTSTRAP_CACHE}"

echo $(date -u) "[Pharo] Reloading rest of packages"
${VM} "${METACELLO_IMAGE_NAME}.image" save "${PHARO_IMAGE_NAME}"

#Terrible HACK!!!! 
#I am increasing the size of the eden space.
#This allows to load the big baselines.
#However, this is only needed because the VM has a bug when extending the space itself. It is corrupting the objects, this produces random crashes
${VM} "${PHARO_IMAGE_NAME}.image" eval --save "Smalltalk vm parameterAt: 45 put: (Smalltalk vm parameterAt: 44) * 4"

env 2>&1 > env.log

${VM} "${PHARO_IMAGE_NAME}.image" eval --save "MCCacheRepository uniqueInstance disable"
#First we load the remote repositories capabilities of Monticello to be able to load the Pharo baseline with external remote dependencies
${VM} "${PHARO_IMAGE_NAME}.image" metacello install --save --signalErrorOnWarning "tonel://${BOOTSTRAP_REPOSITORY}/src" Monticello --groups RemoteRepositories
${VM} "${PHARO_IMAGE_NAME}.image" metacello install --save --signalErrorOnWarning "tonel://${BOOTSTRAP_REPOSITORY}/src" Pharo

#Storing the image version into the image header
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Smalltalk vm saveImageVersionInImageHeader"

#Extending the default number of stack pages.
#The VM is divorcing all frames to free a stackPage if there is not a free one.
#We can check the statistics of number of pages free using the "Smalltalk vm parameterAt: 61"
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Smalltalk vm parameterAt: 43 put: 32"

${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "MCCacheRepository uniqueInstance enable. FFIMethodRegistry resetAll. PharoSourcesCondenser condenseNewSources. Smalltalk garbageCollect"
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" clean --release
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "SystemBuildInfo current initializeForRelease"

${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save "Pharo"
echo "${PHARO_SHORT_VERSION}" > pharo.version

# clean bak sources files
rm -f *.bak

PHARO_SOURCES_PREFIX=$(echo "${PHARO_NAME_PREFIX}" | cut -d'-' -f 1 | cut -d'.' -f 1-2)
zip "${PHARO_IMAGE_NAME}.zip" ${PHARO_IMAGE_NAME}.* ${PHARO_SOURCES_PREFIX}*.sources pharo.version

