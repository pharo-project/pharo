#!/usr/bin/env bash
#
# Cache of the bootstrap step (3-prepare.sh), so builds whose bootstrap
# inputs did not change can skip it.
#
# Usage, from the root of the build (like the other scripts):
#
#   bootstrap-cache.sh save <bundle-dir>
#       Run right after 3-prepare.sh, before 4-installMetacello.sh changes its
#       output. Packs the output into <bundle-dir>/bootstrap-<arch>.zip, next to
#       the list of source packages it read (.manifest) and its key (.key).
#
#   bootstrap-cache.sh restore <bundle-dir>
#       Unpacks the bundle into the bootstrap cache if its key matches this
#       checkout. Exits with 0 on a hit (3-prepare.sh can be skipped), 1 on a
#       miss and 3 when the bundle cannot be unpacked (run 3-prepare.sh, which
#       overwrites what was unpacked).
#
# The key covers the whole bootstrap/ directory, every source package listed
# in the manifest and the architecture. The manifest is written by
# 3-prepare.sh, so the list of packages is never maintained by hand.
#
# BOOTSTRAP_FORCE=1 makes restore always miss.
#
set -e

SCRIPTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P)"
. ${SCRIPTS}/envvars.sh

MANIFEST_NAME=bootstrapSourcePackages.txt

function usage {
	echo "Usage: $0 save|restore <bundle-dir>"
	exit 2
}

# The key of the bootstrap inputs of this checkout, for the packages listed in the manifest $1
function cache_key {
	{
		echo "arch ${BOOTSTRAP_ARCH}"
		echo "bootstrap $(git -C "${BOOTSTRAP_REPOSITORY}" rev-parse HEAD:bootstrap)"
		sort -u "$1" | while read -r package; do
			# A package removed by this checkout gets "missing", which changes the key
			echo "${package} $(git -C "${BOOTSTRAP_REPOSITORY}" rev-parse --verify --quiet "HEAD:src/${package}" || echo missing)"
		done
	} | git hash-object --stdin
}

# Answers the paths of the manifest $1 that have uncommitted changes, which the key does not see
function uncommitted_inputs {
	git -C "${BOOTSTRAP_REPOSITORY}" status --porcelain -- bootstrap $(sed 's|^|src/|' "$1")
}

function miss {
	echo "bootstrap cache MISS: $1"
	exit 1
}

function save {
	local bundle="$1"
	local manifest="${BOOTSTRAP_CACHE}/${MANIFEST_NAME}"
	[ -f "${manifest}" ] || { echo "No ${manifest}: run 3-prepare.sh first"; exit 1; }

	mkdir -p "${bundle}"
	bundle="$(cd "${bundle}" ; pwd -P)"
	rm -f "${bundle}/bootstrap-${BOOTSTRAP_ARCH}".*

	cp "${manifest}" "${bundle}/bootstrap-${BOOTSTRAP_ARCH}.manifest"
	cache_key "${manifest}" > "${bundle}/bootstrap-${BOOTSTRAP_ARCH}.key"
	(cd "${BOOTSTRAP_CACHE}" && zip -r "${bundle}/bootstrap-${BOOTSTRAP_ARCH}.zip" \
		bootstrap.image \
		*.hermes \
		*.txt \
		pharo-local/package-cache)

	echo "bootstrap cache saved: key=$(cat "${bundle}/bootstrap-${BOOTSTRAP_ARCH}.key")"
}

function restore {
	local bundle="$1"
	local zip="${bundle}/bootstrap-${BOOTSTRAP_ARCH}.zip"
	local manifest="${bundle}/bootstrap-${BOOTSTRAP_ARCH}.manifest"
	local key_file="${bundle}/bootstrap-${BOOTSTRAP_ARCH}.key"
	local key

	[ "${BOOTSTRAP_FORCE}" == "1" ] && miss "forced with BOOTSTRAP_FORCE=1"
	[ -f "${zip}" ] && [ -f "${manifest}" ] && [ -f "${key_file}" ] || miss "no bundle in ${bundle}"
	[ -z "$(uncommitted_inputs "${manifest}")" ] || miss "uncommitted changes in the bootstrap inputs"

	key="$(cache_key "${manifest}")"
	[ "${key}" == "$(cat "${key_file}")" ] || miss "key ${key} differs from the bundle's $(cat "${key_file}")"

	mkdir -p "${BOOTSTRAP_CACHE}"
	unzip -o -q "${zip}" -d "${BOOTSTRAP_CACHE}" || { echo "bootstrap cache ERROR: cannot unpack ${zip}"; exit 3; }
	echo "bootstrap cache HIT: key=${key}"
}

[ $# -eq 2 ] || usage
case "$1" in
	save) save "$2" ;;
	restore) restore "$2" ;;
	*) usage ;;
esac
