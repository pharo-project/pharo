#!/bin/bash
set -e
set -x

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.
# where can I find the VM?
VM="./vm/pharo"

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


PREFIX=Pharo7.0

REPOSITORY="${BOOTSTRAP_REPOSITORY:-..}"

if [[ -z "${BOOTSTRAP_REPOSITORY}" ]]; then
  GIT_COMMIT_HASH=$(git show -s --format=%h)
else
  GIT_COMMIT_HASH=$(git -C ${REPOSITORY} show -s --format=%h)
fi

SUFFIX=${ARCH_DESCRIPTION}bit-${GIT_COMMIT_HASH}

if [[ ${DESCRIBE} -eq "1" ]]; then
  echo "${SUFFIX}"
  exit 0
fi

BOOTSTRAP_IMAGE_NAME=bootstrap
BOOTSTRAP_ARCHIVE_IMAGE_NAME=${PREFIX}-bootstrap-${SUFFIX}

HERMES_ARCHIVE_NAME=${PREFIX}-hermesPackages-${SUFFIX}
RPACKAGE_ARCHIVE_NAME=${PREFIX}-rpackage-${SUFFIX}

CORE_IMAGE_NAME=${PREFIX}-core-${SUFFIX}
COMPILER_IMAGE_NAME=${PREFIX}-compiler-${SUFFIX}
TRAITS_IMAGE_NAME=${PREFIX}-traits-${SUFFIX}
MC_BOOTSTRAP_IMAGE_NAME=${PREFIX}-monticello_bootstrap-${SUFFIX}
MC_IMAGE_NAME=${PREFIX}-monticello-${SUFFIX}
METACELLO_IMAGE_NAME=${PREFIX}-metacello-${SUFFIX}
PHARO_IMAGE_NAME=${PREFIX}-${SUFFIX}

CACHE="${BOOTSTRAP_CACHE:-bootstrap-cache}"

VM=./vm/pharo

#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd "${CACHE}"

#We need the old sources file next to the image because of sources condensation step
wget http://files.pharo.org/sources/PharoV60.sources

#Prepare
echo "Prepare Bootstrap files"
cp "${BOOTSTRAP_IMAGE_NAME}.image" "${COMPILER_IMAGE_NAME}.image"

# Archive bootstrap image
cp "${BOOTSTRAP_IMAGE_NAME}.image" "${BOOTSTRAP_ARCHIVE_IMAGE_NAME}.image"
zip "${BOOTSTRAP_ARCHIVE_IMAGE_NAME}.zip" "${BOOTSTRAP_ARCHIVE_IMAGE_NAME}.image"

# Archive binary Hermes packages
zip "${HERMES_ARCHIVE_NAME}.zip" *.hermes

# Archive RPackage definitions
zip "${RPACKAGE_ARCHIVE_NAME}.zip" protocolsKernel.txt packagesKernel.txt

${REPOSITORY}/bootstrap/scripts/download_vm.sh

echo "Prepare icons"
mkdir icon-packs
cd icon-packs
# update the commit hash as soon as you need a new version of the icons to be loaded
wget http://github.com/pharo-project/pharo-icon-packs/archive/57fba57a02ef3b96c453fb9feba7b71c6a3e618e.zip -O idea11.zip
cd ..

# Find st-cache path
[[ -z "${BOOTSTRAP_CACHE}" ]] && ST_CACHE='st-cache' || ST_CACHE="${BOOTSTRAP_CACHE}/st-cache"

# Installing RPackage
echo "[Compiler] Initializing Bootstraped Image"
${VM} "${COMPILER_IMAGE_NAME}.image" # I have to run once the image so the next time it starts the CommandLineHandler.

echo "[Compiler] Adding more Kernel packages"
${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes Hermes-Extensions.hermes --save
${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes Multilingual-Encodings.hermes Multilingual-TextConversion.hermes Multilingual-Languages.hermes --save --no-fail-on-undeclared

${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes InitializePackagesCommandLineHandler.hermes --save

${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes Collections-Atomic.hermes AST-Core.hermes Collections-Arithmetic.hermes Jobs.hermes --save --no-fail-on-undeclared

echo "[Compiler] Initializing the packages in the Kernel"
${VM} "${COMPILER_IMAGE_NAME}.image" initializePackages --protocols=protocolsKernel.txt --packages=packagesKernel.txt --save

# Installing compiler through Hermes 
echo "[Compiler] Installing compiler through Hermes"
${VM} "${COMPILER_IMAGE_NAME}.image" loadHermes OpalCompiler-Core.hermes CodeExport.hermes CodeImport.hermes CodeImportCommandLineHandlers.hermes --save --no-fail-on-undeclared
${VM} "${COMPILER_IMAGE_NAME}.image" eval --save "CompilationContext initialize. OCASTTranslator initialize." 
${VM} "${COMPILER_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/01-initialization/01-init.st --no-source --save --quit

echo "[Compiler] Initializing Unicode"
${VM} "${COMPILER_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/01-initialization/03-initUnicode.st --no-source --save --quit "${REPOSITORY}/resources/unicode/"

${VM} "${COMPILER_IMAGE_NAME}.image" st ${ST_CACHE}/DeprecatedFileStream.st ${ST_CACHE}/FileSystem.st --no-source --quit --save
${VM} "${COMPILER_IMAGE_NAME}.image" eval --save "SourceFileArray initialize"
zip "${COMPILER_IMAGE_NAME}.zip" "${COMPILER_IMAGE_NAME}.image"

# Installing Traits through Hermes 
echo "[Compiler] Installing Traits through Hermes"

${VM} "${COMPILER_IMAGE_NAME}.image" save ${TRAITS_IMAGE_NAME}
${VM} "${TRAITS_IMAGE_NAME}.image" loadHermes TraitsV2.hermes --save
${VM} "${TRAITS_IMAGE_NAME}.image" loadHermes Kernel-Traits.hermes AST-Core-Traits.hermes Collections-Abstract-Traits.hermes Transcript-Core-Traits.hermes SUnit-Core-Traits.hermes CodeImport-Traits.hermes RPackage-Traits.hermes OpalCompiler-Traits.hermes Slot-Traits.hermes CodeExport-Traits.hermes System-Sources-Traits.hermes System-Support-Traits.hermes TraitsV2-Compatibility.hermes --save
zip "${TRAITS_IMAGE_NAME}.zip" "${TRAITS_IMAGE_NAME}.image"

#Bootstrap Initialization: Class and RPackage initialization
echo "[Core] Class and RPackage initialization"
${VM} "${TRAITS_IMAGE_NAME}.image" save ${CORE_IMAGE_NAME}
zip "${CORE_IMAGE_NAME}.zip" "${CORE_IMAGE_NAME}.image"

#Bootstrap Monticello Part 1: Core and Local repositories
echo "[Monticello] Bootstrap Monticello Core and Local repositories"

${VM} "${CORE_IMAGE_NAME}.image" save ${MC_BOOTSTRAP_IMAGE_NAME}
#cp "${CORE_IMAGE_NAME}.image" "${MC_BOOTSTRAP_IMAGE_NAME}.image"
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" st ${ST_CACHE}/Monticello.st --save --quit
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/02-monticello-bootstrap/01-fixLocalMonticello.st --save --quit
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/02-monticello-bootstrap/02-bootstrapMonticello.st --save --quit
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" eval --save "TraitsBootstrap fixSourceCodeOfTraits "
zip "${MC_BOOTSTRAP_IMAGE_NAME}.zip" ${MC_BOOTSTRAP_IMAGE_NAME}.*

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo "[Monticello] Loading Networking Packages and Remote Repositories"
${VM} "${MC_BOOTSTRAP_IMAGE_NAME}.image" save $MC_IMAGE_NAME
${VM} "${MC_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/02-monticello-bootstrap/03-bootstrapMonticelloRemote.st --save --quit
zip "${MC_IMAGE_NAME}.zip" ${MC_IMAGE_NAME}.*

#Bootstrap Metacello
echo "[Metacello] Bootstrapping Metacello"
${VM} "${MC_IMAGE_NAME}.image" save ${METACELLO_IMAGE_NAME}
${VM} "${METACELLO_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
zip "${METACELLO_IMAGE_NAME}.zip" ${METACELLO_IMAGE_NAME}.*

echo "[Pharo] Reloading rest of packages"
${VM} "${METACELLO_IMAGE_NAME}.image" save "${PHARO_IMAGE_NAME}"

# fix the display size in the image header (position 40 [zero based], 24 for 32-bit image)
# in older versions we must use octal representation
printf "\231\002\320\003" > displaySize.bin
if [[ ${ARCH_DESCRIPTION} -eq "32" ]]; then
  SEEK=24
else
  SEEK=40
fi
dd if="displaySize.bin" of="${PHARO_IMAGE_NAME}.image" bs=1 seek=$SEEK count=4 conv=notrunc

#Terrible HACK!!!! 
#I am increasing the size of the eden space.
#This allows to load the big baselines.
#However, this is only needed because the VM has a bug when extending the space itself. It is corrupting the objects, this produces random crashes
${VM} "${PHARO_IMAGE_NAME}.image" eval --save "Smalltalk vm parameterAt: 45 put: (Smalltalk vm parameterAt: 44) * 4"


${VM} "${PHARO_IMAGE_NAME}.image" eval --save "Metacello new baseline: 'Tonel';repository: 'github://pharo-vcs/tonel:v1.0.5';onWarning: [ :e | Error signal: e messageText in: e signalerContext ]; load: 'core'"
${VM} "${PHARO_IMAGE_NAME}.image" eval --save "Metacello new baseline: 'IDE';repository: 'tonel://${REPOSITORY}/src';onWarning: [ :e | Error signal: e messageText in: e signalerContext ]; load"
${VM} "${PHARO_IMAGE_NAME}.image" eval --save "FFIMethodRegistry resetAll. PharoSourcesCondenser condenseNewSources. Smalltalk garbageCollect"
${VM} "${PHARO_IMAGE_NAME}.image" clean --release

echo "[Pharo] Configure resulting image"
${VM} "${PHARO_IMAGE_NAME}.image" st ${REPOSITORY}/bootstrap/scripts/04-configure-resulting-image/fixPackageVersions.st --save --quit

${VM} "${PHARO_IMAGE_NAME}.image" save "Pharo"

# clean bak sources files
rm -f *.bak

zip "${PHARO_IMAGE_NAME}.zip" ${PHARO_IMAGE_NAME}.*
