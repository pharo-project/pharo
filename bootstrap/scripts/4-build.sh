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
MC_IMAGE_NAME=${PHARO_NAME_PREFIX}-monticello-${SUFFIX}
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

# Find st-cache path
[[ -z "${BOOTSTRAP_CACHE}" ]] && ST_CACHE='st-cache' || ST_CACHE="${BOOTSTRAP_CACHE}/st-cache"

# Installing Package
echo $(date -u) "[Compiler] Initializing Bootstraped Image"
${VM} "${COMPILER_IMAGE_NAME}.image" # I have to run once the image so the next time it starts the CommandLineHandler.

echo $(date -u) "[Compiler] Adding more Kernel packages"
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Hermes-Extensions.hermes --save
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Math-Operations-Extensions.hermes Debugging-Core.hermes System-Time.hermes Multilingual-Encodings.hermes ReflectionMirrors-Primitives.hermes --save --no-fail-on-undeclared

# Now that System-Time is loaded, we can initialize the version
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" perform  --save ChronologyConstants initialize
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" perform  --save DateAndTime initialize
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" perform  --save SystemVersion setMajor:minor:patch:suffix:build:commitHash: ${PHARO_MAJOR} ${PHARO_MINOR} ${PHARO_PATCH} ${PHARO_SUFFIX} ${BUILD_NUMBER} ${PHARO_COMMIT_HASH}

${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes InitializePackagesCommandLineHandler.hermes --save

${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Collections-Atomic.hermes AST-Core.hermes Collections-Arithmetic.hermes ClassDefinitionPrinters.hermes System-SourcesCondenser.hermes System-NumberPrinting.hermes --save --no-fail-on-undeclared

echo $(date -u) "[Compiler] Initializing the packages in the Kernel"
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" initializePackages --protocols=protocolsKernel.txt --packages --save

# Installing compiler through Hermes 
echo $(date -u) "[Compiler] Installing compiler through Hermes"
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Debugging-Utils.hermes OpalCompiler-Core.hermes CodeImport.hermes CodeImportCommandLineHandlers.hermes --save --no-fail-on-undeclared
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "OpalCompiler register. CompilationContext initialize. OCASTTranslator initialize."
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "SystemEnvironment deprecatedAliases: { #SystemDictionary }." # This line should be removed in Pharo 14 since it is for backward compatibility.
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/01-initialization/01-init.st --no-source --save --quit

echo $(date -u) "[Compiler] Initializing Unicode"
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/01-initialization/02-initUnicode.st --no-source --save --quit "${BOOTSTRAP_REPOSITORY}/resources/unicode/"

${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Random-Core.hermes System-Hashing.hermes Network-UUID.hermes FileSystem-Core.hermes FileSystem-Disk.hermes --save --no-fail-on-undeclared
${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "PharoBootstrapInitialization initializeFileSystem"
zip "${COMPILER_IMAGE_NAME}.zip" "${COMPILER_IMAGE_NAME}.image"

# Installing Traits through Hermes 
echo $(date -u) "[Compiler] Installing Traits through Hermes"

${VM} "${COMPILER_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save ${TRAITS_IMAGE_NAME}
${VM} "${TRAITS_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Traits.hermes --save
${VM} "${TRAITS_IMAGE_NAME}.image" "${IMAGE_FLAGS}" loadHermes Kernel-Traits.hermes Collections-Abstract-Traits.hermes CodeImport-Traits.hermes --save
zip "${TRAITS_IMAGE_NAME}.zip" "${TRAITS_IMAGE_NAME}.image"

#Bootstrap Initialization: Class and Package initialization
echo $(date -u) "[Core] Class and Package initialization"
${VM} "${TRAITS_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save ${CORE_IMAGE_NAME}
zip "${CORE_IMAGE_NAME}.zip" "${CORE_IMAGE_NAME}.image"

#Bootstrap Monticello Part 1: Core and Local repositories
echo $(date -u) "[Monticello] Bootstrap Monticello Core and Local repositories"

${VM} "${CORE_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save ${MC_BOOTSTRAP_IMAGE_NAME}
#cp "${CORE_IMAGE_NAME}.image" "${MC_BOOTSTRAP_IMAGE_NAME}.image"
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" "${IMAGE_FLAGS}" st ${ST_CACHE}/Monticello.st --save --quit
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" "${IMAGE_FLAGS}" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/02-monticello-bootstrap/01-bootstrapMonticello.st --save --quit
zip "${MC_BOOTSTRAP_IMAGE_NAME}.zip" ${MC_BOOTSTRAP_IMAGE_NAME}.*

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo $(date -u) "[Monticello] Loading Networking Packages and Remote Repositories"
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save $MC_IMAGE_NAME
${VM} "${MC_IMAGE_NAME}.image" "${IMAGE_FLAGS}" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/02-monticello-bootstrap/02-bootstrapMonticelloRemote.st --save --quit
zip "${MC_IMAGE_NAME}.zip" ${MC_IMAGE_NAME}.*

#Bootstrap Metacello
echo "[Metacello] Bootstrapping Metacello"
${VM} "${MC_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save ${METACELLO_IMAGE_NAME}
${VM} "${METACELLO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" st ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
${VM} "${METACELLO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Metacello new baseline: 'Tonel';repository: 'github://pharo-vcs/tonel:Pharo12';onWarning: [ :e | Error signal: e messageText in: e signalerContext ]; load: 'core'"
zip "${METACELLO_IMAGE_NAME}.zip" ${METACELLO_IMAGE_NAME}.*

echo $(date -u) "[Pharo] Reloading rest of packages"
${VM} "${METACELLO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save "${PHARO_IMAGE_NAME}"

#Terrible HACK!!!! 
#I am increasing the size of the eden space.
#This allows to load the big baselines.
#However, this is only needed because the VM has a bug when extending the space itself. It is corrupting the objects, this produces random crashes
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Smalltalk vm parameterAt: 45 put: (Smalltalk vm parameterAt: 44) * 4"

env 2>&1 > env.log

${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "MCCacheRepository uniqueInstance disable"
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Metacello new baseline: 'Pharo';repository: 'tonel://${BOOTSTRAP_REPOSITORY}/src';onWarning: [ :e | Error signal: e messageText in: e signalerContext ]; load"

#Storing the image version into the image header
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Smalltalk vm saveImageVersionInImageHeader"

#Extending the default number of stack pages.
#The VM is divorcing all frames to free a stackPage if there is not a free one.
#We can check the statistics of number of pages free using the "Smalltalk vm parameterAt: 61"
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "Smalltalk vm parameterAt: 43 put: 32"

${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" eval --save "MCCacheRepository uniqueInstance enable. FFIMethodRegistry resetAll. PharoSourcesCondenser condenseNewSources. Smalltalk garbageCollect"
${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" clean --release

${VM} "${PHARO_IMAGE_NAME}.image" "${IMAGE_FLAGS}" save "Pharo"
echo "${PHARO_SHORT_VERSION}" > pharo.version

# clean bak sources files
rm -f *.bak

# delete Pharo60 sources files
rm PharoV60.sources*

PHARO_SOURCES_PREFIX=$(echo "${PHARO_NAME_PREFIX}" | cut -d'-' -f 1 | cut -d'.' -f 1-2)
zip "${PHARO_IMAGE_NAME}.zip" ${PHARO_IMAGE_NAME}.* ${PHARO_SOURCES_PREFIX}*.sources pharo.version

