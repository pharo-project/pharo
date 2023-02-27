# Pharo

This repository contains sources of the [Pharo language](http://pharo.org/). Pharo is a pure object-oriented programming language and a powerful environment, focused on simplicity and immediate feedback (think IDE and OS rolled into one).

![Pharo 6 screenshot](https://pbs.twimg.com/media/DBpdIGrXkAA8SJ1.jpg)

## Download Pharo

To download the Pharo stable version for your platform, please visit:

- [http://pharo.org/download](http://pharo.org/download)

## Virtual machine

This repository contains only sources of the Pharo image. The virtual machine source code is managed in a separate repository:

- [https://github.com/pharo-project/pharo-vm/](https://github.com/pharo-project/pharo-vm/)

## Automated Builds

This repository is being built on a [Jenkins server](https://ci.inria.fr/pharo-ci-jenkins2) and uploaded to [files.pharo.org](https://files.pharo.org).

- [Latest build - 64bit](http://files.pharo.org/image/110/latest-64.zip)
- [Latest build - 32bit](http://files.pharo.org/image/110/latest.zip)

The minimal image contains the basic Pharo packages without the graphical user interface. It is useful as a base for server-side applications deployment.

- [Minimal image latest build - 64bit](http://files.pharo.org/image/110/latest-minimal-64.zip)
- [Minimal image latest build - 32bit](http://files.pharo.org/image/110/latest-minimal-32.zip)


## Bootstrapping Pharo from sources

To bootstrap a new Pharo image you need the latest stable version of Pharo. For more information about bootstrapping, refer to [guillep/PharoBootstrap](https://github.com/guillep/PharoBootstrap).

The bootstrapping can be done on a properly-named branch using the following script:

```bash
./bootstrap/scripts/bootstrap.sh
```

This will generate and archive images at various stages of the bootstrap process up to the full image in `Pharo11.0-64bit-hhhhhhh.zip` where hhhhhhh is the hash of the current checkout. Additional information on the stages of the bootstrap and how to snapshot during the process are provided as comments in bootstrap.sh.

* You can set the `BUILD_NUMBER` environment variable to to a unique integer (this is typically used only for the [official builds](https://files.pharo.org/image/110/) and will default to `0` if not specified).
* You can set the `BOOTSTRAP_ARCH` environment variable to either `64` (the default) or `32`.
* You can set the `BOOTSTRAP_REPOSITORY` and `BOOTSTRAP_CACHE` environment variables to do the bootstrap outside of the source repository.
* You can set the `BOOTSTRAP_VMTARGET` environment variable to make the bootstrap use a virtual machine already present in your system (otherwise it will download it).
* If you are on a branch that doesn't follow the expected naming convention ('`PharoX.Y`'), then the script will pick an appropriate default (such as `Pharo11.0`). To build Pharo11.0 from a custom branch, you need to set `BRANCH_NAME=Pharo11` before the bootstrap script is run.

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

Pharo is an opensource project very friendly to contributions of the users. See the document [CONTRIBUTING](CONTRIBUTING.md) how you can help to improve Pharo.

## Pharo friendly links and organisations

[http://github.com/Pharo-project/PharoMap](http://github.com/Pharo-project/PharoMap)
