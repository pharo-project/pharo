#!/bin/bash
#
# Perform the entire bootstrap process.
#
# For a list of required and optional input environment variables
# please see envvars.sh
#
# The current working directory must be the pharo project git
# root directory
#
# The goal of breaking the process in to the 4 stages below is to facilitate
# manual development, e.g. when testing a new VM:
#
# 1. bootstrap/scripts/1-clean.sh
# 2. bootstrap/scripts/2-download.sh
# 3. Replace default target VM with dev version
# 4. bootstrap/scripts/3-prepare.sh
# 5. bootstrap/scripts/backup.sh
# 6. bootstrap/scripts/4-build.sh
# 7. on error:
#       bootstrap/scripts/restore.sh
#       goto 6
#
set -x
set -e

source bootstrap/scripts/envvars.sh

SCRIPTS=${REPOSITORY}/bootstrap/scripts

#
# Remove any artifacts from previous runs
#
${SCRIPTS}/1-clean.sh
#
# Fetch all prerequisites
#
${SCRIPTS}/2-download.sh
#
# Prepare the bootstrap environment
#
${SCRIPTS}/3-prepare.sh
#
# Build the new image
#
${SCRIPTS}/4-build.sh

