#!/usr/bin/env bash
# Bash3 Boilerplate. Copyright (c) 2014, kvz.io

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

# Since there is random failure during tests execution we print the content of the current directory to find potential problems
ls -lAh