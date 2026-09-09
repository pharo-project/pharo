# Pharo

This repository contains sources of the [Pharo language](http://pharo.org/). Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one). Visit us on https://www.pharo.org or [Pharo Discord Server](https://discord.gg/QewZMZa)

<p align="center">
  <a href="https://www.pharo.org">
    <img alt="Pharo" src="https://img.shields.io/static/v1?style=for-the-badge&message=Pharo&color=3297d4&logo=Harbor&logoColor=FFFFFF&label=" />
  </a>
  <a href="https://discord.gg/QewZMZa">  
    <img alt="Discord online members" src="https://img.shields.io/discord/223421264751099906?color=5865F2&label=Discord&style=for-the-badge" />
  </a>
  <img alt="Commit activity per month" src="https://img.shields.io/github/commit-activity/m/pharo-project/pharo?style=for-the-badge" />
</p>

![Pharo screenshot](https://pbs.twimg.com/media/DBpdIGrXkAA8SJ1.jpg)

## Download Pharo

To download the Pharo stable version for your platform, please visit:

- [http://pharo.org/download](http://pharo.org/download)

## Virtual machine

This repository contains only sources of the Pharo image. The virtual machine source code is managed in a separate repository:

- [https://github.com/pharo-project/pharo-vm/](https://github.com/pharo-project/pharo-vm/)

## Automated Builds

This repository is being built on a [Jenkins server](https://ci.inria.fr/pharo-ci-jenkins2) and uploaded to [files.pharo.org](https://files.pharo.org).

- [Latest build - 64bit](http://files.pharo.org/image/120/latest-64.zip)
- [Latest build - 32bit](http://files.pharo.org/image/120/latest.zip)

The minimal image contains the basic Pharo packages without the graphical user interface. It is useful as a base for server-side applications deployment.

- [Minimal image latest build - 64bit](http://files.pharo.org/image/120/latest-minimal-64.zip)
- [Minimal image latest build - 32bit](http://files.pharo.org/image/120/latest-minimal-32.zip)


## Bootstrapping Pharo from sources

To bootstrap a new Pharo image you need the latest stable version of Pharo. For more information about bootstrapping, refer to [guillep/PharoBootstrap](https://github.com/guillep/PharoBootstrap).

The bootstrapping can be done on a properly-named branch using the following script:

```bash
./bootstrap/scripts/bootstrap.sh
```

This will generate and archive images at various stages of the bootstrap process up to the full image in `Pharo12.0-64bit-hhhhhhh.zip` where hhhhhhh is the hash of the current checkout. Additional information on the stages of the bootstrap and how to snapshot during the process are provided as comments in bootstrap.sh.

* You can set the `BUILD_NUMBER` environment variable to a unique integer (this is typically used only for the [official builds](https://files.pharo.org/image/120/) and will default to `0` if not specified).
* You can set the `BOOTSTRAP_ARCH` environment variable to either `64` (the default) or `32`.
* You can set the `BOOTSTRAP_REPOSITORY` and `BOOTSTRAP_CACHE` environment variables to do the bootstrap outside of the source repository.
* You can set the `BOOTSTRAP_VMTARGET` environment variable to make the bootstrap use a virtual machine already present in your system (otherwise it will download it).
* If you are on a branch that doesn't follow the expected naming convention ('`PharoX.Y`'), then the script will pick an appropriate default (such as `Pharo12.0`). To build Pharo12.0 from a custom branch, you need to set `BRANCH_NAME=Pharo12` before the bootstrap script is run.

### Bootstrapping with Docker

You can also use Docker if you prefer to control the bootstrapping environment completely. The following `Dockerfile` provides a Docker image with a fresh build. You can repeat the command `git pull && ./bootstrap/scripts/bootstrap.sh` in a container at any time:

```Dockerfile
# docker build --tag pharo .
# docker run --rm --name pharo -it pharo
FROM ubuntu:22.04
RUN apt-get update && apt-get -y install build-essential git wget zip
RUN git clone https://github.com/pharo-project/pharo.git /root/pharo
WORKDIR /root/pharo
RUN git pull && ./bootstrap/scripts/bootstrap.sh
ENTRYPOINT [ "bash" ]
```

Alternatively, in the root directory of this project (after a `git clone`), you can set up a Docker volume pointing to the project directory and build from within the Docker container (nice for Windows environments!):

```bash
# in the host environment start a Docker container
docker run --rm -it -v $(pwd)/:/pharo --workdir /pharo ubuntu:22.04 bash
# in the container add the required packages and start the bootstrap script
apt-get update && apt-get -y install build-essential git wget zip
./bootstrap/scripts/bootstrap.sh
```

## File format

This source code repository is exported in [Tonel format](https://github.com/pharo-vcs/tonel). In this format, packages are represented as directories and each class is inside a single file.

## How to contribute

Pharo is an open source project very friendly to contributions of the users. See the document [CONTRIBUTING](CONTRIBUTING.md) how you can help to improve Pharo.

## Pharo friendly links and organizations

[http://github.com/Pharo-project/PharoMap](http://github.com/Pharo-project/PharoMap)

## Stargazers over time
[![Stargazers over time](https://starchart.cc/pharo-project/pharo.svg?background=%23FFFFFF&axis=%23333333&line=%232f81f7)](https://starchart.cc/pharo-project/pharo)
