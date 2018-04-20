# Pharo

This repository contains sources of the [Pharo language](http://pharo.org/). Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one).

![Pharo 6 screenshot](https://pbs.twimg.com/media/DBpdIGrXkAA8SJ1.jpg)

## Download Pharo

To download the Pharo stable version for your platform, please visit:

- [http://pharo.org/download](http://pharo.org/download)

## Virtual machine

This repository contains only sources of the Pharo image. The virtual machine is served by a separate repository:

- [https://github.com/OpenSmalltalk/opensmalltalk-vm](https://github.com/OpenSmalltalk/opensmalltalk-vm)

## Automated Builds

This repository is being built on a [Jenkins server](https://ci.inria.fr/pharo-ci-jenkins2) and uploaded to [files.pharo.org](https://files.pharo.org).

- [Latest build - 64bit](http://files.pharo.org/image/70/latest-64.zip)
- [Latest build - 32bit](http://files.pharo.org/image/70/latest.zip) 

The minimal image contains the basic Pharo packages without the graphical user interface. It is useful as a base for server-side applications deployment.

- [Minimal image latest build - 64bit](http://files.pharo.org/image/70/latest-minimal-64.zip)
- [Minimal image latest build - 32bit](http://files.pharo.org/image/70/latest-minimal-32.zip) 


## Bootstrapping Pharo from sources

To bootstrap a new Pharo image you need the latest stable version of Pharo. For more information about bootstrapping, refer to [guillep/PharoBootstrap](https://github.com/guillep/PharoBootstrap).

The bootstrapping can be done using the following script:

```bash
BUILD_NUMBER=42 BOOTSTRAP_ARCH=32 sh ./bootstrap/scripts/bootstrap.sh
```

This will generate and archive images at various stages of the bootstrap process up to the full image in Pharo7.0-32bit-hhhhhhh.zip where hhhhhhh is the identifying hash.

Additional information on the stages of the bootstrap and how to snapshot during the process are provided as comments in bootstrap.sh.

__Tip:__ You can set `BOOTSTRAP_REPOSITORY` and `BOOTSTRAP_CACHE` environment variables to do the bootstrap outside of the source repository.


## File format

This source code repository is exported in [Tonel format](https://github.com/pharo-vcs/tonel). In this format, packages are represented as directories and each class is inside a single file.

## How to contribute

Pharo is an opensource project very friendly to contributions of the users. See the document [CONTRIBUTING](CONTRIBUTING.md) how you can help to improve Pharo.
