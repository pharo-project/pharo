#!/bin/sh

PWD="$( dirname $(readlink -f $0) )"

echo "GET LATEST PHARO"
wget --quiet -O - get.pharo.org/60+vm | bash
VERSION=`./pharo Pharo.image eval SystemVersion current highestUpdate`

echo "GOT ${VERSION}!"

echo "INSTALL GIT FILETREE SUPPORT"
./pharo Pharo.image --save --quit ${PWD}/pre_install_exporter.st

echo "EXPORTING SOURCE CODE"
./pharo Pharo.image --save --quit ${PWD}/export.st

echo "GIT COMMIT, TAG, AND PUSH"
git add *
git commit -m "EXPORT VERSION ${VERSION}"
git tag -a v${VERSION} -m "TAG VERSION ${VERSION}"
git push --force --tags git@github.com:guillep/pharo-core.git