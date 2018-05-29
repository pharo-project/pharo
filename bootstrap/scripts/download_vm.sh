set -x
set -e

#Downloads a SPUR vm for the configured architecture

mkdir vm
cd vm
echo `pwd`

../bootstrap/scripts/getPharoVM.sh 70 vm $BOOTSTRAP_ARCH

cd ..
