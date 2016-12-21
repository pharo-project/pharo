#Bootstrap Initialization: Class and RPackage initialization
echo "Bootstrap Initialization: Class and RPackage initialization"
rm bootstrap.image && cp bootstrap.image.backup bootstrap.image && rm PharoDebug.log
./pharo bootstrap.image st bootstrap/scripts/01-initialization/init.st --save --quit
./pharo bootstrap.image st bootstrap/scripts/01-initialization/initRPackageOrganizer.st --save --quit

#Bootstrap Monticello Part 1: Core and Local repositories
echo "Bootstrap Monticello Part 1: Core and Local repositories"
./pharo bootstrap.image st bootstrap-cache/st-cache/Monticello.st --save --quit
./pharo bootstrap.image st bootstrap/scripts/02-monticello-bootstrap/fixLocalMonticello.st --save --quit
./pharo bootstrap.image st bootstrap/scripts/02-monticello-bootstrap/loadMonticello.st --save --quit

#Bootstrap Monticello Part 2: Networking Packages and Remote Repositories
echo "Bootstrap Monticello Part 2: Networking Packages and Remote Repositories"
rm -rf pharo-local/package-cache/*
cp bootstrap-cache/mcz-cache/* pharo-local/package-cache
./pharo bootstrap.image st bootstrap/scripts/02-monticello-bootstrap/loadMonticelloRemote.st --save --quit

#Bootstrap Gofer
