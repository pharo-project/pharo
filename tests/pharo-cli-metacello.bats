#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "metacello --help prints help" {
  run_pharo metacello --help
  assert_output --partial "Commands:"
  assert_output --partial "install     Load and install code using Metacello"
  assert_success
}


@test "metacello install --help prints help" {
  run_pharo metacello install --help
  assert_output --partial "Usage: metacello install [--help] [--groups <groups-value>] [--strict] [--signalErrorOnWarning] [--save] [--keepAlive] [--no-quit] [--rename] [<repository>] [<project>]"
  assert_success
}

@test "metacello can install a baseline project" {
  copy_image "test-metacello-load.image"
  run_pharo_with_timeout 200 metacello install --save github://DuneSt/MaterialColors:master/src MaterialColors
  assert_success

  # Check that the loading is ok
  run_pharo eval "Smalltalk at: #MDLColor"
  assert_success
  assert_line "MDLColor"
}