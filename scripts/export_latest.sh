#!/bin/sh

PWD="$( dirname $(readlink -f $0) )"
wget --quiet -O - get.pharo.org/60+vm | bash
./pharo Pharo.image --save ${PWD}/pre_install_exporter.st

$VERSION=`./pharo Pharo.image eval SystemVersion current highestUpdate`
git add *
git commit -m "EXPORT VERSION ${VERSION}"
git tag -a v${VERSION} -m "TAG VERSION ${VERSION}"
git push --force --tags