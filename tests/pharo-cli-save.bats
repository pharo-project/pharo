#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "save --help prints help" {
  run_pharo save --help
  assert_output --partial "Usage: save [--help] [--delete-old] [<imageName>]"
  assert_success
}

@test "test can save to a new image name" {
  copy_image "test-save-cmd.image"
  local changes_file="${IMAGE%.image}.changes"

  run_pharo save newname

  assert_success
  assert_file_exists "$WORKDIR"/newname.image
  assert_file_exists "$WORKDIR"/newname.changes
  assert_file_exists "$IMAGE"
  assert_file_exists "$changes_file"
}

@test "test can save to a new image name and delete old image" {
  copy_image "test-save-cmd.image"
  local changes_file="${IMAGE%.image}.changes"

  run_pharo save --delete-old newname

  assert_success
  assert_file_exists "$WORKDIR"/newname.image
  assert_file_exists "$WORKDIR"/newname.changes
  refute_file_exists "$IMAGE"
  refute_file_exists "$changes_file"
}

@test "Save without providing an image name fails" {
  run_pharo save

  assert_failure
  assert_line --index 0 --partial "Image name is mandatory"
}