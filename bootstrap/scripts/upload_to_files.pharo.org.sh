#! /bin/bash

set -ex

destDir="/appli/files.pharo.org/image/70/"
echo "Uploading Images to pharo.files.org/$destDir"
scp -o StrictHostKeyChecking=no -v \
  latest*.zip \
  Pharo-7.*.zip \
  Pharo-metacello*.zip \
    pharoorgde@ssh.cluster023.hosting.ovh.net:files/image/70/

scp -o StrictHostKeyChecking=no -v \
  Pharo-bootstrap*.zip \
  Pharo-core*.zip \
  Pharo-compiler*.zip \
  Pharo-monticello*.zip \
  Pharo-rpackage*.zip \
  Pharo-hermesPackages*.zip \
    pharoorgde@ssh.cluster023.hosting.ovh.net:files/image/70/bootstrap/
