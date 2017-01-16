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

GIT_DESCRIBE=`git describe --always`
SUFFIX=${ARCH_DESCRIPTION}bit-${GIT_DESCRIBE}

if [[ ${DESCRIBE} -eq "1" ]]; then
  echo ${SUFFIX}
  exit 0
fi

SUFFIX=-$SUFFIX

#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd bootstrap-cache

#Prepare
echo "Prepare Bootstrap files"
unzip ../resources/fonts/BitmapDejaVuSans.fuel -d .
cp bootstrap.image core.image
../bootstrap/scripts/download_vm.sh


#Bootstrap Initialization: Class and RPackage initialization
echo "[Core] Class and RPackage initialization"
./vm/pharo -vm-display-null core.image st ../bootstrap/scripts/01-initialization/01-init.st --save --quit
./vm/pharo -vm-display-null core.image st ../bootstrap/scripts/01-initialization/02-initRPackageOrganizer.st --save --quit
zip core$SUFFIX.zip core.image

#Bootstrap Monticello Part 1: Core and Local repositories
echo "[Monticello] Bootstrap Monticello Core and Local repositories"
./vm/pharo -vm-display-null core.image save monticello_bootstrap
./vm/pharo -vm-display-null monticello_bootstrap.image st st-cache/Monticello.st --save --quit
./vm/pharo -vm-display-null monticello_bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/01-fixLocalMonticello.st --save --quit
./vm/pharo -vm-display-null monticello_bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/02-bootstrapMonticello.st --save --quit
zip monticello_bootstrap$SUFFIX.zip monticello_bootstrap.*

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo "[Monticello] Loading Networking Packages and Remote Repositories"
./vm/pharo -vm-display-null monticello_bootstrap.image save monticello
./vm/pharo -vm-display-null monticello.image st ../bootstrap/scripts/02-monticello-bootstrap/03-bootstrapMonticelloRemote.st --save --quit
zip monticello$SUFFIX.zip monticello.*

#Bootstrap Metacello
echo "[Metacello] Bootstrapping Metacello"
./vm/pharo -vm-display-null monticello.image save metacello
./vm/pharo -vm-display-null metacello.image st ../bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
zip metacello$SUFFIX.zip metacello.*

echo "[Pharo] Reloading rest of packages"
./vm/pharo -vm-display-null metacello.image save Pharo
ln -s .. pharo-core #Required for the correct work of metacello baselines
./vm/pharo -vm-display-null Pharo.image eval "Metacello new baseline: 'IDE';repository: 'filetree://../src'; load"
zip Pharo$SUFFIX.zip Pharo.*
