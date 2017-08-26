# Installing Pharo VM on Linux

When running Pharo in production you might want to install the image and a VM from distribution packages. So far there were no current public packages and during the last months I have modernized the debian packaging and recently added CentOS rpm packaging as well. The process of generating new source packages is integrated into the pharo-vm travis-ci build as well.

Every time that master of pharo-vm.git is updated a new source package will be uploaded to the Open(SUSE) Build Service and new packages will be built and are available in a "latest" package feed. The generated (and used) sourcecode is inside the source package and will be archived by the OBS platform (one can even nicely diff versions). The "stable" VM is not built from pharo-vm.git (yet?) so it doesn't show on OBS yet.

Below are some semi-tested installation notes in case you want to try it out. The binary names are picked to allow to install the 32bit and 64bit VM in parallel. The names currently are:

        - pharo6-32             (32bit version)
        - pharo6-64             (64bit version)
        - pharo6-32-ui          (32bit version with X11 dependencies)
        - pharo6-64-ui          (64bit version with X11 dependencies)


CentOS 6.8:

# Add the repo
$ yum-config-manager --add-repo http://download.opensuse.org/repositories/devel:/languages:/pharo:/latest/CentOS_6/devel:languages:pharo:latest.repo

# Install 32bit packages (with X11 dependency for *-ui or not)

$ yum install pharo6-32-ui.i686 or pharo6-32.i386

# Install 64bit packages

$ yum install pharo6-64-ui.x86_64 pharo6-64.x86_64



CentOS 7 (only 64bit):


$ yum-config-manager --add-repo http://download.opensuse.org/repositories/devel:/languages:/pharo:/latest/CentOS_7/devel:languages:pharo:latest.repo
$ yum install pharo6-64-ui.x86_64 pharo6-64.x86_64


Debian 8:

# Add signing key
$ wget -O - http://download.opensuse.org/repositories/devel:/languages:/pharo:/latest/Debian_8.0/Release.key | apt-key add -

# Update and install
$ apt update
$ apt install pharo6-32-ui pharo6-64-ui    (or pharo6-32 or pharo6-64)

Ubuntu 14.04 LTS

# Add signing key
$ wget -O - http://download.opensuse.org/repositories/devel:/languages:/pharo:/latest/xUbuntu_14.04/Release.key | apt-key add -

# Update and install
$ apt update
$ apt install pharo6-32-ui pharo6-64-ui

Ubuntu 16.04

# Add signing key
$ wget -O - http://download.opensuse.org/repositories/devel:/languages:/pharo:/latest/xUbuntu_16.04/Release.key | apt-key add -

# Update and install
$ apt update
$ apt install pharo6-32-ui pharo6-64-ui
