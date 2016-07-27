# Pharo-Core repository

This repository contains a mirror of the packages part of the [Pharo language](http://pharo.org/). Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one).

## Bootstrapping Pharo from sources

[![Build Status](https://travis-ci.org/guillep/pharo-core.svg?branch=master)](https://travis-ci.org/guillep/pharo-core)

This source code repository serves also for bootstrapping the latest version of the [Pharo language](http://pharo.org/). The script in *scripts/bootstrap.st* serves for this purpose. For more information about bootstrapping, refer to [guillep/PharoBootstrap](https://github.com/guillep/PharoBootstrap).
```
[ Metacello new
	baseline: 'Iceberg';
	repository: 'github://npasserini/iceberg';
	load: 'development'.

Metacello new
	baseline: 'PharoBootstrapProcess';
	repository: 'github://guillep/PharoBootstrap';
	load ] on: Warning do: #resume.
	
#PBBootstrapSpur5032bit asClass new
	gitRepositoryUrl: 'git@github.com:guillep/pharo-core.git'
	location: '.'
	subdirectory: 'src';
	espellBackend: #EPSimulatorBackend asClass for32Bit forBootstrap;
	bootstrap
```

## File format

This source code repository is exported in [FileTree metadataless format](https://github.com/dalehenrich/filetree). In this format, packages and classes are represented as directories. Each method is inside a single file.

## Keeping this repository up to date

This repository is a mirrored version of the latest Pharo packages. The scripts folder contains the scripts to export a Pharo image's source code.

- export_latest.sh
- export.st

A CI job in the [Inria](http://ci.inria.fr) infrastructure executes these scripts for every new version of the source code and pushes into this repository the latest version of the sources.

https://ci.inria.fr/pharo/view/Pharo%20bootstrap/job/Pharo-6.0-Bootstrap-Git-Export/