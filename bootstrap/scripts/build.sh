#!/usr/bin/bash
set -e

#Get inside the bootstrap-cache folder. Pharo interprets relatives as relatives to the image and not the 'working directory'
cd bootstrap-cache

#Prepare
echo "Prepare Bootstrap files"
unzip ../resources/fonts/BitmapDejaVuSans.fuel -d .
cp bootstrap.image core.image


#Bootstrap Initialization: Class and RPackage initialization
echo "[Core] Class and RPackage initialization"
../pharo core.image st ../bootstrap/scripts/01-initialization/01-init.st --save --quit
../pharo core.image st ../bootstrap/scripts/01-initialization/02-initRPackageOrganizer.st --save --quit
zip core.zip core.image

#Bootstrap Monticello Part 1: Core and Local repositories
echo "[Monticello] Bootstrap Monticello Core and Local repositories"
../pharo core.image save monticello_bootstrap
../pharo monticello_bootstrap.image st st-cache/Monticello.st --save --quit
../pharo monticello_bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/01-fixLocalMonticello.st --save --quit
../pharo monticello_bootstrap.image st ../bootstrap/scripts/02-monticello-bootstrap/02-bootstrapMonticello.st --save --quit
zip monticello_bootstrap.zip monticello_bootstrap.*

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo "[Monticello] Loading Networking Packages and Remote Repositories"
../pharo monticello_bootstrap.image save monticello
../pharo monticello.image st ../bootstrap/scripts/02-monticello-bootstrap/03-bootstrapMonticelloRemote.st --save --quit
zip monticello.zip monticello.*

#Bootstrap Metacello
echo "[Metacello] Bootstrapping Metacello"
../pharo monticello.image save metacello
../pharo metacello.image st ../bootstrap/scripts/03-metacello-bootstrap/01-loadMetacello.st --save --quit
zip metacello.zip metacello.*

echo "[Pharo] Reloading rest of packages"
../pharo metacello.image save Pharo
ln -s .. pharo-core #Required for the correct work of metacello baselines
../pharo Pharo.image eval "Metacello new baseline: 'IDE';repository: 'filetree://../src'; load"
zip Pharo.zip Pharo.*
