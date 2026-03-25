#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "clean --help prints help" {
  run_pharo clean --help
  assert_output --partial "Usage: clean [--help] [--production] [--release]"
  assert_success
}

@test "clean can run basic clean on image" {
  skip
  copy_image "test-clean.image"
  # Add a breakpoint that should be cleaned with basic image cleanup
  run_pharo eval --save "Breakpoint breakOn: #browseAll inObject: Breakpoint"
  assert_success
  run_pharo eval "Breakpoint all size"
  assert_success
  assert_line "1"

  run_pharo_with_timeout 10 clean
  assert_success
  # ensure Breakpoints are cleaned
  run_pharo eval "Breakpoint all size"
  assert_success
  assert_line "0"
}

@test "clean can run release clean" {
  copy_image "test-clean.image"
  # create an empty package that should be cleaned by the command
  run_pharo eval --save 'PackageOrganizer default addPackage: #empty'
  assert_success
  run_pharo eval 'PackageOrganizer default hasPackage: #empty'
  assert_success
  assert_line "true"

  run_pharo_with_timeout 30 clean --release
  assert_success
  # ensure packages are cleaned
  run_pharo eval 'PackageOrganizer default hasPackage: #empty'
  assert_success
  assert_line "false"
}

@test "clean can run production clean" {
  # clean --production takes too much time to run
  skip

  copy_image "test-clean.image"
  run_pharo_with_timeout 30 clean --production
  assert_success
  # ensure packages are cleaned
  run_pharo eval "PackageOrganizer default hasPackage: 'MonticelloMocks'"
  assert_success
  assert_line "false"
}