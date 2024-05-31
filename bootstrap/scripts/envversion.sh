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
	branchName=$(echo "${BRANCH_NAME}" | grep -E "^Pharo[0-9]+(\.[0-9]+)?\$")
	if [ "${branchName}" == "" ]; then
		echo 0
	else
		echo 1
	fi
}

function set_version_common() {
	# HACK: Since this may beforehand a PR branch, I do not have all information I need. I assume I will have a tag indicating Pharo version.
	# Note: do not use `--first-parent` here, since Jenkins is crazy and use the PR as the first parent wheras useful version information in the targeted branch.
    # Set the common env vars extracting version information. Requires that prefix and suffix are set beforehand
    PHARO_MAJOR="$(git describe --tags --first-parent | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-1)"
    PHARO_MINOR="$(git describe --tags --first-parent | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 2-2)"
    PHARO_PATCH="$(git describe --tags --first-parent | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 3-3)"

	# This will answer "Pharo7.0-PR"
	PHARO_NAME_PREFIX="Pharo${PHARO_MAJOR}.${PHARO_MINOR}-${PHARO_SUFFIX}"
    # This will answer "70"
	PHARO_SHORT_VERSION="${PHARO_MAJOR}${PHARO_MINOR}"
}

# sets variables when we are in a release build
function set_version_release_variables() {
	# I'm a release, I have all values needed in a TAG
	# This will answer "Pharo7.0"
    
    PHARO_SUFFIX=""
    set_version_common
}

# sets variables when we are in a snapshot build
function set_version_snapshot_variables() {
	# ensure we have BRANCH_NAME variable
	ensure_branch_name
	# This will answer "Pharo7.0-SNAPSHOT"
    PHARO_SUFFIX="SNAPSHOT"
    set_version_common
}

# sets variables when we are in a pull request build
function set_version_pull_request_variables() {
	# I'm not development build, I should be a PR
    PHARO_SUFFIX="PR"
    set_version_common
}

# sets all variables:
# 
#  PHARO_NAME_PREFIX -> Prefix to name the buids (Pharo7.0.0-rc1, Pharo7.0-SNAPSHOT, Pharo7.0.0-PR)
#  PHARO_SHORT_VERSION -> Short version of the image (70, 80, etc.)
#  PHARO_VM_VERSION -> VM version (equivallent to PHARO_SHORT_VERSION)
#
# Input environment variables:
#  BOOTSTRAP_REPOSITORY - the root directory of the git repository
function set_version_variables() {

	pushd "$BOOTSTRAP_REPOSITORY" > /dev/null
	if [ $(is_development_build) == 1 ]; then
		if [ $(is_release_build) == 1 ]; then
			set_version_release_variables
		else
			set_version_snapshot_variables
		fi
	else
		set_version_pull_request_variables
	fi
	
	PHARO_VM_VERSION=${PHARO_SHORT_VERSION}
	PHARO_COMMIT_HASH="$(git rev-parse --verify HEAD)"
	popd > /dev/null
} 

