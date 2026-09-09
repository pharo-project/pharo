#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "fuel --help prints help" {
  run_pharo fuel --help
  assert_success
  assert_output --partial "Usage: fuel [--help] [--save] [--keepAlive] [<FILE>]"
}

@test "fuel can load a fuel file" {
  copy_image "test-fuel.image"
  run_pharo eval "FLSerializer new
	object: 'stringToSerialize';
	filePath: '$WORKDIR/exported-empty-package.fuel';
    serialize"
  assert_file_exists $WORKDIR/exported-empty-package.fuel

  run_pharo fuel $WORKDIR/exported-empty-package.fuel --save
  assert_success
}

# We need to test that fuel can load a fuel file and save before quitting
# but we have no testable use case for now

@test "fuel outputs error if no fuel file provided" {
  run_pharo fuel foo.bar
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*Missing Fuel file as argument[[:blank:]]*"
}