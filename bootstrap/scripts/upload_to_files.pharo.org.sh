#! /bin/bash

set -ex

destDir="/appli/files.pharo.org/image/70/"
echo "Uploading Images to pharo.files.org/$destDir"
scp -o StrictHostKeyChecking=no -v *.zip filepharosync@file-pharo.inria.fr:/appli/files.pharo.org/image/70/