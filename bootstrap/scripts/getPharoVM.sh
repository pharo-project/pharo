#!/usr/bin/env bash

if [ $# -lt 1 ]
  then
    echo "This script needs at least one argument: a Pharo version."
    echo "Example: 61"
    echo "An optional argument can be a vm description as vm or vmT. By default it will be vm."
    echo "Example: 61 vmT"
    echo "An optional argument can be the architecture of the system. 32 or 64 bits. By default it will be 32bits."
    echo "Example: 61 vm 64"
    echo "A last optional argument can be the number of retry in case something is wrong. By default it will be 3."
    echo "Example: 61 vm 64 2"
    exit 1
fi

# Express arguments in a more reabable way
export PHARO=$1
export VM=${2-vm}
export ARCHITECTURE=${3-32}
export RETRY_REMAINING=${4-3}

#Zero conf does not have yet an url get.pharo.org/32/... This explain why we need to use a condition to get a Pharo 32 or 64 bits.
if [ $ARCHITECTURE = 32 ]; then
  wget --quiet -O - get.pharo.org/${VM}${PHARO} | bash
else
  wget --quiet -O - get.pharo.org/$ARCHITECTURE/${VM}${PHARO} | bash
fi

#If the exit of the previous command is not 0 (sucess), retry after cleaning
if [ $? -eq 0 ]
then
  echo "VM downloaded"
else
  echo "Download failed"
  rm -rf pharo pharo-ui pharo-vm/
  echo "Remaining retries: " $RETRY_REMAINING
  if [ $RETRY_REMAINING -gt 0 ]
  then
    echo "Retry"
    ./$0 $PHARO $VM $ARCHITECTURE `expr $RETRY_REMAINING - 1`
  else
    echo "Failed to download the VM"
  fi
fi