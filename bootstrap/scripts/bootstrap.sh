#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset
# set -o xtrace

# Set magic variables for current file & dir
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
__file="${__dir}/$(basename "${BASH_SOURCE[0]}")"
__base="$(basename ${__file} .sh)"

BOOTSTRAP_ARCH="${1:-32}"

wget https://github.com/guillep/PharoBootstrap/releases/download/v1.1/bootstrapImage.zip
wget -O - get.pharo.org/vm60 | bash

unzip bootstrapImage.zip
./pharo Pharo.image ${__dir}/prepare_image.st --save --quit

./pharo Pharo.image ${__dir}/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --quit
bash ${__dir}/build.sh