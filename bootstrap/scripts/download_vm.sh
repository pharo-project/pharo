set -x
set -e

#Downloads a SPUR vm for the configured architecture

mkdir vm
cd vm
echo `pwd`

if [ ${BOOTSTRAP_ARCH} = "64" ]; then
	ARCHFLAG=64/
fi

wget -O- get.pharo.org/${ARCHFLAG}vm60 | bash
cd ..