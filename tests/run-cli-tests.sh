#!/usr/bin/env bash
set -euo pipefail

BATS_VERSION="1.12.0"

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
VENDOR_DIR="$ROOT_DIR/vendor"
mkdir -p "$VENDOR_DIR"

# -------------------------------
# Helper function: download and extract a tarball if missing
# -------------------------------
download_extract() {
  local name="$1"
  local url="$2"
  local dest="$VENDOR_DIR/$name"

  if [ ! -d "$dest" ]; then
    echo "Downloading $name..."
    curl -L -o "$VENDOR_DIR/$name.tar.gz" "$url"
    mkdir -p "$dest"
    tar -xzf "$VENDOR_DIR/$name.tar.gz" -C "$dest" --strip-components=1
  fi
}

# -------------------------------
# Helper function: check that timeout util is installed
# -------------------------------
check_timeout_command() {
  if ! command -v timeout >/dev/null 2>&1; then
    echo >&2 "❌ The 'timeout' tool (from GNU CoreUtils) is required by this test suite."
    echo >&2 "   Please install it, for example with:"
    echo >&2 "     brew install coreutils"
    echo >&2 "   On some systems, the binary may be called 'gtimeout'."
    exit 1
  fi
}

# -------------------------------
# Install bats, bats-support and bats-assert
# -------------------------------

download_extract "bats" \
  "https://github.com/bats-core/bats-core/archive/refs/tags/v${BATS_VERSION}.tar.gz"

download_extract "bats-support" \
  "https://github.com/bats-core/bats-support/archive/refs/tags/v0.3.0.tar.gz"

download_extract "bats-assert" \
  "https://github.com/bats-core/bats-assert/archive/refs/tags/v2.2.0.tar.gz"

# -------------------------------
# Detect OS type
# -------------------------------
detect_os() {
    OS_TYPE="$(uname)"
    case "$OS_TYPE" in
        Darwin)
            echo "macos"
            ;;
        Linux)
            echo "linux"
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

check_timeout_command

# Setting env
BATS_DIR="$VENDOR_DIR/bats"
BATS_SUPPORT="$VENDOR_DIR/bats-support"
BATS_ASSERT="$VENDOR_DIR/bats-assert"
detect_os

if [ -z "${PHARO:-}" ]; then
  PHARO="./pharo"
fi
if [ -z "${IMAGE:-}" ]; then
  IMAGE="Pharo.image"
fi


export BATS_SUPPORT
export BATS_ASSERT
export ROOT_DIR
export PHARO
export IMAGE
export OS_TYPE
echo ">> Running tests with bats-core"
# To enable debug information, you can add these options: --show-output-of-passing-tests --verbose-run
BATS_OPTIONS="--print-output-on-failure --report-formatter junit"
COMMAND_LINE_HANDLER=ClapCommandLineHandler "$BATS_DIR/bin/bats" $BATS_OPTIONS "$ROOT_DIR"/pharo-cli-perform/pharo-cli-perform.bats
COMMAND_LINE_HANDLER=PerformMessageCommandLineHandler "$BATS_DIR/bin/bats" $BATS_OPTIONS "$ROOT_DIR"/pharo-cli-perform/pharo-cli-perform.bats
"$BATS_DIR/bin/bats" $BATS_OPTIONS "$ROOT_DIR"