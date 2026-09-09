#!/usr/bin/env bash

set -x

if [ $# -lt 1 ]
  then
    echo "This script needs at least one argument: a Pharo version."
    echo "Example: 61"
    echo "A last optional argument can be the number of retry in case something is wrong. By default it will be 3."
    echo "Example: 61 vm 64 2"
    exit 1
fi

# Express arguments in a more reabable way
export PHARO=$1
export RETRY_REMAINING=${3-3}

wget --quiet -O - get.pharo.org/vm${PHARO} | bash

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
    $0 $PHARO `expr $RETRY_REMAINING - 1`
  else
    echo "Failed to download the VM"
  fi
fi
