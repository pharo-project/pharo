#! /bin/bash

set -ex

destDir="/appli/files.pharo.org/image/70/"
echo "Uploading Images to pharo.files.org/$destDir"
scp *.zip files.pharo.org:$destDir