#! /bin/bash

set -ex

destDir="/appli/files.pharo.org/image/70/"
echo "Uploading Images to pharo.files.org/$destDir"
scp -o StrictHostKeyChecking=no -v \
  latest*.zip \
  Pharo-7.*.zip \
    filepharosync@file-pharo.inria.fr:/appli/files.pharo.org/image/70/

scp -o StrictHostKeyChecking=no -v \
  Pharobootstrap*.zip \
  Pharocore*.zip \
  Pharocompiler*.zip \
  Pharomonticello_bootstrap*.zip \
  Pharomonticello*.zip \
  Pharorpackage*.zip \
  PharohermesPackages*.zip \
    filepharosync@file-pharo.inria.fr:/appli/files.pharo.org/image/70/bootstrap/
