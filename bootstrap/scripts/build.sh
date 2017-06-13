#!/bin/bash
set -e

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

if [ -z ${ARCH_DESCRIPTION} ]; then
  echo "No architecture specified. Please set the BOOTSTRAP_ARCH environment variable or use the -a argument";
  exit 1;
fi


PREFIX=Pharo7.0

GIT_COMMIT_HASH=`git show -s --format=%h`
SUFFIX=${ARCH_DESCRIPTION}bit-${GIT_COMMIT_HASH}

if [[ ${GIT_COMMIT_HASH} -eq "1" ]]; then
  echo ${SUFFIX}
  exit 0
fi

BOOTSTRAP_IMAGE_NAME=bootstrap
CORE_IMAGE_NAME=${PREFIX}-core-${SUFFIX}
MC_BOOTSTRAP_IMAGE_NAME=${PREFIX}-monticello_bootstrap-${SUFFIX}
MC_IMAGE_NAME=${PREFIX}-monticello-${SUFFIX}
METACELLO_IMAGE_NAME=${PREFIX}-metacello-${SUFFIX}
PHARO_IMAGE_NAME=${PREFIX}-${SUFFIX}


#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd bootstrap-cache

#We need the old sources file next to the image because of sources condensation step
wget http://files.pharo.org/sources/PharoV60.sources

#Prepare
echo "Prepare Bootstrap files"
cp $BOOTSTRAP_IMAGE_NAME.image $CORE_IMAGE_NAME.image
../bootstrap/scripts/download_vm.sh

echo "Prepare fonts"
unzip ../resources/fonts/BitmapDejaVuSans.fuel -d .

echo "Prepare icons"
mkdir icon-packs
cd icon-packs
wget http://github.com/pharo-project/pharo-icon-packs/archive/idea11.zip
cd ..

#Required for the correct work of metacello baselines and unicode initialization
ln -s .. pharo-core 

#Bootstrap Initialization: Class and RPackage initialization
echo "[Core] Class and RPackage initialization"
./vm/pharo $CORE_IMAGE_NAME.image st ../bootstrap/scripts/01-initialization/01-init.st --save --quit
./vm/pharo $CORE_IMAGE_NAME.image st ../bootstrap/scripts/01-initialization/02-initRPackageOrganizer.st --save --quit
./vm/pharo $CORE_IMAGE_NAME.image st ../bootstrap/scripts/01-initialization/03-initUnicode.st --save --quit
zip $CORE_IMAGE_NAME.zip $CORE_IMAGE_NAME.image

#Bootstrap Monticello Part 1: Core and Local repositories
echo "[Monticello] Bootstrap Monticello Core and Local repositories"
./vm/pharo $CORE_IMAGE_NAME.image save $MC_BOOTSTRAP_IMAGE_NAME
./vm/pharo $MC_BOOTSTRAP_IMAGE_NAME.image st st-cache/Monticello.st --save --quit
./vm/pharo $MC_BOOTSTRAP_IMAGE_NAME.image st ../bootstrap/scripts/02-monticello-bootstrap/01-fixLocalMonticello.st --save --quit
./vm/pharo $MC_BOOTSTRAP_IMAGE_NAME.image st ../bootstrap/scripts/02-monticello-bootstrap/02-bootstrapMonticello.st --save --quit
zip $MC_BOOTSTRAP_IMAGE_NAME.zip $MC_BOOTSTRAP_IMAGE_NAME.*

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo "[Monticello] Loading Networking Packages and Remote Repositories"
./vm/pharo $MC_BOOTSTRAP_IMAGE_NAME.image save $MC_IMAGE_NAME
./vm/pharo $MC_IMAGE_NAME.image st ../bootstrap/scripts/02-monticello-bootstrap/03-bootstrapMonticelloRemote.st --save --quit
zip $MC_IMAGE_NAME.zip $MC_IMAGE_NAME.*

#Bootstrap Metacello
echo "[Metacello] Bootstrapping Metacello"
./vm/pharo $MC_IMAGE_NAME.image save $METACELLO_IMAGE_NAME
./vm/pharo $METACELLO_IMAGE_NAME.image st ../bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
zip $METACELLO_IMAGE_NAME.zip $METACELLO_IMAGE_NAME.*

echo "[Pharo] Reloading rest of packages"
./vm/pharo $METACELLO_IMAGE_NAME.image save ${PHARO_IMAGE_NAME}
./vm/pharo "${PHARO_IMAGE_NAME}.image" eval --save "Metacello new baseline: 'IDE';repository: 'filetree://../src'; load"
./vm/pharo "${PHARO_IMAGE_NAME}.image" eval --save "FFIMethodRegistry resetAll. PharoSourcesCondenser condenseNewSources"
./vm/pharo "${PHARO_IMAGE_NAME}.image" clean --release

# clean bak sources files
rm -f *.bak

# fix the display size in the image header (position 40 [zero based], 24 for 32-bit image)
# in older versions we must use octal representation
printf "\231\002\320\003" > displaySize.bin
if [[ ${ARCH_DESCRIPTION} -eq "32" ]]; then
  SEEK=24
else
  SEEK=40
fi
dd if="displaySize.bin" of="${PHARO_IMAGE_NAME}.image" bs=1 seek=$SEEK count=4 conv=notrunc

zip ${PHARO_IMAGE_NAME}.zip ${PHARO_IMAGE_NAME}.*
