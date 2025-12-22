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
  This script assumes the existence of a new uninitialized image. Then it proceeds to its initialization and \"growing\".
  * Step 1:
    - Initialize the image
    - output: core.image
  * Step 2:
    - Bootstrap Monticello local repositories
    - output: monticello_bootstrap.image and changes file
  * Step 3:
    - Load Monticello remote repositories
    - output: monticello.image and changes file
  * Step 4:
    - Load Metacello
    - output: metacello.image and changes file
  * Step 5:
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

BOOTSTRAP_IMAGE_NAME=bootstrap
BOOTSTRAP_ARCHIVE_IMAGE_NAME=${PHARO_NAME_PREFIX}-bootstrap-${SUFFIX}

HERMES_ARCHIVE_NAME=${PHARO_NAME_PREFIX}-hermesPackages-${SUFFIX}
RPACKAGE_ARCHIVE_NAME=${PHARO_NAME_PREFIX}-rpackage-${SUFFIX}

CORE_IMAGE_NAME=${PHARO_NAME_PREFIX}-core-${SUFFIX}
COMPILER_IMAGE_NAME=${PHARO_NAME_PREFIX}-compiler-${SUFFIX}
TRAITS_IMAGE_NAME=${PHARO_NAME_PREFIX}-traits-${SUFFIX}
MC_BOOTSTRAP_IMAGE_NAME=${PHARO_NAME_PREFIX}-monticello_bootstrap-${SUFFIX}
METACELLO_IMAGE_NAME=${PHARO_NAME_PREFIX}-metacello-${SUFFIX}
PHARO_IMAGE_NAME=${PHARO_NAME_PREFIX}-${SUFFIX}

#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd "${BOOTSTRAP_CACHE}"

#Prepare
echo "Prepare Bootstrap files"
cp "${BOOTSTRAP_IMAGE_NAME}.image" "${COMPILER_IMAGE_NAME}.image"

# Archive bootstrap image
cp "${BOOTSTRAP_IMAGE_NAME}.image" "${BOOTSTRAP_ARCHIVE_IMAGE_NAME}.image"
zip "${BOOTSTRAP_ARCHIVE_IMAGE_NAME}.zip" "${BOOTSTRAP_ARCHIVE_IMAGE_NAME}.image"

# Archive binary Hermes packages
zip "${HERMES_ARCHIVE_NAME}.zip" *.hermes

# Archive Package definitions
zip "${RPACKAGE_ARCHIVE_NAME}.zip" protocolsKernel.txt

# Installing Package
echo $(date -u) "[Compiler] Initializing Bootstraped Image and fixing the code"
${VM} "${COMPILER_IMAGE_NAME}.image" # I have to run once the image so the next time it starts the CommandLineHandler.
${VM} "${COMPILER_IMAGE_NAME}.image" perform --save PharoBootstrapFixMethodsTool fixMethodsIn: protocolsKernel.txt # Fixing some things espell does not handel well
${VM} "${COMPILER_IMAGE_NAME}.image" perform --save PharoBootstrapFixMethodsTool fixExtensionMethods

echo $(date -u) "[Compiler] Adding more Kernel packages"
echo "Loading packages: $(cat hermesAdditionalKernelPackages.txt)"
${VM} "${COMPILER_IMAGE_NAME}.image" perform --save BasicHermesTool load: --as-array $(cat hermesAdditionalKernelPackages.txt)

# Now that System-Version is loaded, we can initialize the version
${VM} "${COMPILER_IMAGE_NAME}.image" perform  --save SystemVersion setMajor:minor:patch:suffix:build:commitHash: ${PHARO_MAJOR} ${PHARO_MINOR} ${PHARO_PATCH} ${PHARO_SUFFIX} ${BUILD_NUMBER} ${PHARO_COMMIT_HASH}

# Installing compiler through Hermes 
echo $(date -u) "[Compiler] Installing compiler through Hermes"
echo "Loading packages: $(cat hermesCompilerPackages.txt)"
${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes $(cat hermesCompilerPackages.txt) --save --no-fail-on-undeclared
${VM} "${COMPILER_IMAGE_NAME}.image" eval --save "SystemEnvironment deprecatedAliases: { #SystemDictionary }." # This line should be removed in Pharo 14 since it is for backward compatibility.
${VM} "${COMPILER_IMAGE_NAME}.image" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/01-initialization/01-init.st --no-source --save --quit

echo $(date -u) "[Compiler] Initializing Unicode"
${VM} "${COMPILER_IMAGE_NAME}.image" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/01-initialization/02-initUnicode.st --no-source --save --quit "${BOOTSTRAP_REPOSITORY}/resources/unicode/"

echo "Loading packages: $(cat hermesFileSystemPackages.txt)"
${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes $(cat hermesFileSystemPackages.txt) --save --no-fail-on-undeclared
zip "${COMPILER_IMAGE_NAME}.zip" "${COMPILER_IMAGE_NAME}.image"

# Installing Traits through Hermes 
echo $(date -u) "[Compiler] Installing Traits through Hermes"

${VM} "${COMPILER_IMAGE_NAME}.image" save ${TRAITS_IMAGE_NAME}
echo "Loading packages: $(cat hermesTraitsPackages.txt)"
${VM} "${TRAITS_IMAGE_NAME}.image" loadHermes $(cat hermesTraitsPackages.txt) --save
zip "${TRAITS_IMAGE_NAME}.zip" "${TRAITS_IMAGE_NAME}.image"

#Bootstrap Initialization: Class and Package initialization
echo $(date -u) "[Core] Class and Package initialization"
${VM} "${TRAITS_IMAGE_NAME}.image" save ${CORE_IMAGE_NAME}
zip "${CORE_IMAGE_NAME}.zip" "${CORE_IMAGE_NAME}.image"

#Bootstrap Monticello Part 1: Core and Local repositories
echo $(date -u) "[Monticello] Bootstrap Monticello Core and Local repositories"

${VM} "${CORE_IMAGE_NAME}.image" save ${MC_BOOTSTRAP_IMAGE_NAME}
echo "Loading packages: $(cat hermesMonticelloPackages.txt)"
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" loadHermes $(cat hermesMonticelloPackages.txt) --save --no-fail-on-undeclared
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/02-monticello-bootstrap/01-bootstrapMonticello.st --save --quit
zip "${MC_BOOTSTRAP_IMAGE_NAME}.zip" ${MC_BOOTSTRAP_IMAGE_NAME}.*

#Bootstrap Metacello
echo "[Metacello] Bootstrapping Metacello"
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" save ${METACELLO_IMAGE_NAME}
${VM} "${METACELLO_IMAGE_NAME}.image" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
git clone https://github.com/pharo-vcs/tonel.git -b "Pharo${PHARO_MAJOR}" "${BOOTSTRAP_CACHE}/tonel"
${VM} "${METACELLO_IMAGE_NAME}.image" metacello install --save --signalErrorOnWarning "filetree://${BOOTSTRAP_CACHE}/tonel" Tonel --groups core
#We need the next line because we will reload Tonel from github and this could cause some trouble later
${VM} "${METACELLO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "MetacelloProjectRegistration resetRegistry"
zip "${METACELLO_IMAGE_NAME}.zip" ${METACELLO_IMAGE_NAME}.*

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

# delete Pharo60 sources files
rm PharoV60.sources*

PHARO_SOURCES_PREFIX=$(echo "${PHARO_NAME_PREFIX}" | cut -d'-' -f 1 | cut -d'.' -f 1-2)
zip "${PHARO_IMAGE_NAME}.zip" ${PHARO_IMAGE_NAME}.* ${PHARO_SOURCES_PREFIX}*.sources pharo.version

