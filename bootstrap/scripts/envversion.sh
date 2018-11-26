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
    if [ "${versionTag}" == "" ]; then
        echo 0
    else
        echo 1
    fi
}

# answers if we are in development branch
function is_development_build() {
	set -f 
	local branchName=$(git branch | grep \* | cut -d' ' -f 2 | grep -E "^dev-[0-9]+\.[0-9]+\$")
	if [ "${branchName}" == "" ]; then
		echo 0
	else
		echo 1
	fi
}

# sets all variables
# PHARO_NAME_PREFIX -> Prefix to name the buids (Pharo7.0.0-rc1, Pharo7.0-SNAPSHOT, Pharo7.0.0-PR)
# PHARO_SHORT_VERSION -> Short version of the image (70, 80, etc.)
# PHARO_VM_VERSION -> VM version (equivallent to PHARO_SHORT_VERSION)
function set_version_variables() {
	
	if [ $(is_development_build) == 1 ]; then
		if [ $(is_release_build) == 1 ]; then
			# I'm a release, I have all values needed in a TAG
			PHARO_NAME_PREFIX="Pharo$(git describe --long --tags | cut -d'-' -f 1-2 | cut -c 2-)"
			PHARO_SHORT_VERSION="$(git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//')"
		else
			# I'm not a release, but I'm in development branch. I'm a SNAPSHOT
			PHARO_NAME_PREFIX="Pharo$(git branch | grep \* | cut -d' ' -f 2 | cut -d'-' -f 2)-SNAPSHOT"
			PHARO_SHORT_VERSION="$(git branch | grep \* | cut -d' ' -f 2 | cut -d'-' -f 2 | sed 's/\.//')"
		fi
	else
		# I'm not development build, I should be a PR
		# HACK: Since this is a PR branch, I do not have all information I need. I assume I will have a tag indicating Pharo version.
		PHARO_NAME_PREFIX="Pharo$(git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2)-PR"
		PHARO_SHORT_VERSION="$(git describe --long --tags | cut -d'-' -f 1 | cut -c 2- | cut -d'.' -f 1-2 | sed 's/\.//')"
	fi
	
	# this is just to make things clear (and because they could change in the future, who knows :P)
	PHARO_VM_VERSION="${PHARO_SHORT_VERSION}"
} 