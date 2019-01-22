#!/usr/bin/env bash
#
# Set up Pharo based versions. 
# 
# - current image version prefix, e.g. "Pharo7.0.0-rc1"
# - vm version for the current image, to be used to download the correct vm, e.g. "70"

# answers if we are in a release commit (tagged with vMAJOR.MINOR.PATCH[-EXTRA])
function is_release_build() {
	set -f
	local versionTag=$(git tag --list --points-at HEAD | grep -E "^v[0-9]+\.[0-9]+.[0-9]+(\-[a-zA-Z0-9_]+)?$")
    set +f
	if [ "${versionTag}" == "" ]; then
        echo 0
    else
        echo 1
    fi
}

# Ensures a variable BRANCH_NAME exits.
function ensure_branch_name() {
	# Since jenkins does not checkout the branch but a specific commit (which is completely logical, btw) 
	# git based way to determine branch is not valid. But jenkins provides an environment variable with the 
	# actual branch and we can use that to know. To provide compatibility, in case I do not have a BRANCH_NAME
	# I try to determine it using git.
	# This will be used below.
	if [ "${BRANCH_NAME}" == "" ]; then
		set -f
		BRANCH_NAME="$(git branch | grep \* | cut -d' ' -f 2)"
	    set +f
	fi
}

# answers if we are in development branch
# development branchs have the format PharoMAJOR.MINOR (e.g. Pharo7.0)
function is_development_build() {
	local branchName

	# ensure we have BRANCH_NAME variable
	ensure_branch_name
	# verify match
	branchName=$(echo "${BRANCH_NAME}" | grep -E "^Pharo[0-9]+\.[0-9]+\$")
	if [ "${branchName}" == "" ]; then
		echo 0
	else
		echo 1
	fi
}

# sets variables when we are in a release build
function set_version_release_variables() {
	# I'm a release, I have all values needed in a TAG
	# This will answer "Pharo7.0"
	PHARO_NAME_PREFIX="Pharo$(git describe --long --tags | cut -d'-' -f 1-2 | cut -c 2-)"
	# This will be "70"
	PHARO_SHORT_VERSION="$(git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//')"
}

# sets variables when we are in a snapshot build
function set_version_snapshot_variables() {
	# ensure we have BRANCH_NAME variable
	ensure_branch_name
	# This will answer "Pharo7.0-SNAPSHOT"
	PHARO_NAME_PREFIX="${BRANCH_NAME}-SNAPSHOT"
	# This will answer "70"
	PHARO_SHORT_VERSION="$(echo ${BRANCH_NAME} | cut -c 6- | sed 's/\.//')"
}

# sets variables when we are in a pull request build
function set_version_pull_request_variables() {
	# I'm not development build, I should be a PR
	# HACK: Since this is a PR branch, I do not have all information I need. I assume I will have a tag indicating Pharo version.
	# This will answer "Pharo7.0-PR"
	PHARO_NAME_PREFIX="Pharo$(git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2)-PR"
	# This will answer "70"
	PHARO_SHORT_VERSION="$(git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//')"
}

# sets all variables:
# 
#  PHARO_NAME_PREFIX -> Prefix to name the buids (Pharo7.0.0-rc1, Pharo7.0-SNAPSHOT, Pharo7.0.0-PR)
#  PHARO_SHORT_VERSION -> Short version of the image (70, 80, etc.)
#  PHARO_VM_VERSION -> VM version (equivallent to PHARO_SHORT_VERSION)
function set_version_variables() {
	
	if [ $(is_development_build) == 1 ]; then
		if [ $(is_release_build) == 1 ]; then
			set_version_release_variables
		else
			set_version_snapshot_variables
		fi
	else
		set_version_pull_request_variables
	fi
	
	# I will use 70 vm for now (we still do not have 80 vm)
	#PHARO_VM_VERSION="${PHARO_SHORT_VERSION}"
	PHARO_VM_VERSION="70"
	PHARO_COMMIT_HASH="$(git rev-parse --verify HEAD)"
} 