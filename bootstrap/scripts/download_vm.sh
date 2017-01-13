#Downloads a SPUR vm for the configured architecture

mkdir vm
cd vm
echo `pwd`
wget http://files.pharo.org/vm/pharo-spur${BOOTSTRAP_ARCH}/linux/latest-threaded.zip
unzip latest-threaded.zip
cd ..
