wget -O - get.pharo.org/vm60 | bash
wget https://github.com/guillep/PharoBootstrap/releases/download/v1.1/bootstrapImage.zip
unzip bootstrapImage.zip

./pharo Pharo.image ./bootstrap/scripts/prepare_image.st --save --quit
./pharo Pharo.image ./bootstrap/scripts/generateHermesFiles.st --quit
./pharo Pharo.image ./bootstrap/scripts/bootstrap.st --ARCH=${BOOTSTRAP_ARCH} --quit

# I have to run once the image so the next time it starts the CommandLineHandler.

./pharo bootstrap-cache/bootstrap.image

#bash ./bootstrap/scripts/build.sh