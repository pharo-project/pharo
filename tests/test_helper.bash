load "$BATS_SUPPORT/load"
load "$BATS_ASSERT/load"

setup-workdir() {
  WORKDIR="$ROOT_DIR/cli-tests-tmp"
  mkdir -p "$WORKDIR"
}

teardown-workdir() {
  rm -rf "$WORKDIR"
}

kill-process-or-process-group() {
  if [[ "$OS_TYPE" == "Darwin" ]]; then
    kill-process
  else
    kill-process-group
  fi
}

kill-process() {
  if [[ -n "$pid" ]]; then
    kill -9 "$pid" 2>/dev/null || true
  fi
}

kill-process-group() {
  if [[ -n "$pgid" ]]; then
    kill -9 -"$pgid" 2>/dev/null || true
  fi 
}

run_with_timeout() {
  local seconds="$1"
  shift
  run timeout "$seconds" "$@"
}

run_pharo() {
  run_with_timeout 2 "$PHARO" --headless "$IMAGE" "$@"
}

# Run Pharo as a standalone process. Keeps it pid and pgid to have the ability to kill it. 
# On Mac Os, killing a process group does not work but is needed on Linux
run_pharo_in_backgroud() {
  if [[ "$OS_TYPE" == "Darwin" ]]; then
    (
      exec "$PHARO" --headless "$IMAGE" "$@"
    ) &
  else
    (
      setsid "$PHARO" --headless "$IMAGE" "$@"
    ) &
  fi
  pid=$!
  pgid=$(ps -o pgid= -p "$pid" | tr -d ' ')
}

# Copy a Pharo image + changes file to a new name inside $WORKDIR
# and override $IMAGE just for the current test.
# Usage: copy_image "my-test.image"
copy_image() {
  local dest_name="$1"
  local dest_image="$WORKDIR/$dest_name"
  local dest_changes="${dest_image%.image}.changes"

  # Copy .image
  cp "$IMAGE" "$dest_image"

  # Copy .changes if present
  local src_changes="${IMAGE%.image}.changes"
  if [ -f "$src_changes" ]; then
    cp "$src_changes" "$dest_changes"
  fi

  # Override IMAGE (local to the test’s subshell)
  IMAGE="$dest_image"
}

# Assert that a given PID is running
assert_is_running() {
  local pid="$1"

  if kill -0 "$pid" 2>/dev/null; then
    # success
    return 0
  else
    # fail with a nice error message
    fail "Expected process with PID $pid to be running, but it is not."
  fi
}