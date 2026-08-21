#!/bin/bash

set -x

function showHelp {
  echo
  "./testStrictInstall.sh Clap

  This script loads Core and Tests groups of BaselineOfClap in strict mode
  "
  exit 0
}

RED="\033[0;31m"
YELLOW="\033[0;33m"
DEFAULT="\033[00m"

function myLog {
    echo -e "${YELLOW}$1${DEFAULT}"
}

if [ $# -lt 1 ]; then
    echo "This script needs at least one argument: name of baseline to load."
    echo "Example: Clap"
    showHelp
fi


SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envvars.sh # provides: BOOTSTRAP_REPOSITORY BOOTSTRAP_CACHE VM_BOOTSTRAP
SRC_DIR="${BOOTSTRAP_CACHE}/../.."

BOOTSTRAP_CACHE="${BOOTSTRAP_REPOSITORY}/build/bootstrap-cache"
VM_BOOTSTRAP="${BOOTSTRAP_REPOSITORY}/build/bootstrap-downloads/vmBootstrap/pharo --headless"
VM_TARGET="${BOOTSTRAP_REPOSITORY}/build/bootstrap-downloads/vmtarget/pharo --headless"
SRC_DIR="${BOOTSTRAP_REPOSITORY}/src"

myLog "BOOTSTRAP_REPOSITORY = $BOOTSTRAP_REPOSITORY"
myLog "SRC_DIR = $SRC_DIR"
myLog "BOOTSTRAP_CACHE = $BOOTSTRAP_CACHE"
myLog "VM_BOOTSTRAP = $VM_BOOTSTRAP"
myLog "VM_TARGET = $VM_TARGET"

PROJECT_NAME=$1

if [ ! -e "${BOOTSTRAP_CACHE}/metacello.image" ]; then
    myLog "Create metacello.image"
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/1-clean.sh
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/2-download.sh
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/3-prepare.sh
    ${BOOTSTRAP_REPOSITORY}/bootstrap/scripts/4-installMetacello.sh
    # echo -e "${RED}Should clean and rebuild metacello.image${DEFAULT}"
    # unzip metacello.zip
fi

if [ ! -e "${BOOTSTRAP_CACHE}/sunit.image" ]; then
    myLog "Loading Sunit Core"
    rm -fr sunit.{image,changes}
    ${VM_TARGET} metacello.image save sunit
    ${VM_TARGET} sunit.image metacello install --save --strict --signalErrorOnWarning filetree://${SRC_DIR} SUnit --groups Core
fi

cd ${BOOTSTRAP_CACHE}

rm -fr ${PROJECT_NAME}.{image,changes}
${VM_TARGET} sunit.image save ${PROJECT_NAME}

myLog "Loading ${PROJECT_NAME} Core"
${VM_TARGET} ${PROJECT_NAME}.image metacello install --save --strict --signalErrorOnWarning filetree://${SRC_DIR} ${PROJECT_NAME} --groups Core

myLog "Loading ${PROJECT_NAME} Tests"
${VM_TARGET} ${PROJECT_NAME}.image metacello install --save --strict --signalErrorOnWarning filetree://${SRC_DIR} --groups Tests ${PROJECT_NAME}

myLog "Execute ${PROJECT_NAME} Tests"
${VM_TARGET} ${PROJECT_NAME}.image test --junit-xml-output --project-name ${PROJECT_NAME}