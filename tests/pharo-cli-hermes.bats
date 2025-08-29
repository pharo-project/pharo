#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "loadHermes --help prints help" {
  run_pharo loadHermes --help
  assert_success
  assert_output --partial "Usage: loadHermes [--help] [--no-fail-on-undeclared] [--on-duplication <on-duplication-value>] [--save] [<FILE>]"
}

@test "loadHermes can load a hermes file and quit" {
  copy_image "test-hermes.image"
  run_pharo loadHermes $ROOT_DIR/exported-empty-package.hermes --save
  assert_success

  run_pharo eval Package named: "#empty"
  assert_success
  refute_output --partial "Wednesday"
}

@test "loadHermes outputs error if no hermes file provided" {
  run_pharo loadHermes foo.bar
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*Missing Hermes file as argument[[:blank:]]*"
}