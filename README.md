# Pharo-Core repository

This repository contains a mirror of the packages part of the [Pharo language](http://pharo.org/). Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one).

## Automated Builds

This repository is periodically built in travis and its results uploaded to travis.

[![Build Status](https://travis-ci.org/guillep/pharo-core.svg?branch=master)](https://travis-ci.org/guillep/pharo-core)
[![Latest Nightly Build-32bit](https://api.bintray.com/packages/pharo-project/pharo/Pharo32bit/images/download.svg) ](https://bintray.com/pharo-project/pharo/Pharo32bit/_latestVersion)
[![Latest Nightly Build-64bit](https://api.bintray.com/packages/pharo-project/pharo/Pharo64bit/images/download.svg) ](https://bintray.com/pharo-project/pharo/Pharo64bit/_latestVersion)

You can also programatically load the latest nightly build using the link:

	https://bintray.com/pharo-project/pharo/Pharo/_latestVersion

## Bootstrapping Pharo from sources

This source code repository serves also for bootstrapping the latest version of the [Pharo language](http://pharo.org/). The script in *scripts/bootstrap.st* serves for this purpose. For more information about bootstrapping, refer to [guillep/PharoBootstrap](https://github.com/guillep/PharoBootstrap).
```
[ Metacello new
	baseline: 'Iceberg';
	repository: 'github://npasserini/iceberg:dev-0.4';
	load.

Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'filetree://bootstrap/src';
	load.
] on: Warning do: #resume

(PBBootstrap forArchitecture: '32' "or '64'")
	prepareBootstrap;
	createImage
```

This will generate a new image file named `bootstrap.image` in directory bootstrap-cache.

You should afterwards execute:

```bash
$ ./bootstrap/scripts/build.sh
```

## File format

This source code repository is exported in [FileTree metadataless format](https://github.com/dalehenrich/filetree). In this format, packages and classes are represented as directories. Each method is inside a single file.

## Keeping this repository up to date

This repository is a mirrored version of the latest Pharo packages. The scripts folder contains the scripts to export a Pharo image's source code.

- export_latest.sh
- export.st

A CI job in the [Inria](http://ci.inria.fr) infrastructure executes these scripts for every new version of the source code and pushes into this repository the latest version of the sources.

https://ci.inria.fr/pharo/view/Pharo%20bootstrap/job/Pharo-6.0-Bootstrap-Git-Export/
