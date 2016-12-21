#!/usr/bin/bash
set -e

#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd bootstrap-cache

#Prepare
echo "Prepare Bootstrap files"
unzip ../resources/fonts/BitmapDejaVuSans.fuel -d .

#Bootstrap Initialization: Class and RPackage initialization
echo "Bootstrap Initialization: Class and RPackage initialization"
../pharo bootstrap.image st ../bootstrap/scripts/01-initialization/01-init.st --save --quit
../pharo bootstrap.image st ../bootstrap/scripts/01-initialization/02-initRPackageOrganizer.st --save --quit

#Bootstrap Monticello Part 1: Core and Local repositories
echo "Bootstrap Monticello Part 1: Core and Local repositories"
../pharo bootstrap.image st st-cache/Monticello.st --save --quit
../pharo bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/01-fixLocalMonticello.st --save --quit
../pharo bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/02-bootstrapMonticello.st --save --quit

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo "Bootstrap Monticello Part 2: Networking Packages and Remote Repositories"
../pharo bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/03-bootstrapMonticelloRemote.st --save --quit

#Bootstrap Metacello
echo "Bootstrap Metacello"
../pharo bootstrap.image st ../bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
