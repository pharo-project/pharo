#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

# File exported-empty-package.hermes generated with:
#
# writer := HEBinaryReaderWriter new
#		stream: (File openForWriteFileNamed: 'exported-empty-package.hermes');
#		yourself.
# (HEPackage for: (Package named: 'empty')) writeInto: writer.
# writer flush.

@test "loadHermes --help prints help" {
  run_pharo loadHermes --help
  assert_success
  assert_output --partial "Usage: loadHermes [--help] [--no-fail-on-undeclared] [--on-duplication <on-duplication-value>] [--save] [<FILE>]"
}

@test "loadHermes can load a hermes file and save before quitting" {
  copy_image "test-hermes.image"

  run_pharo eval PackageOrganizer default hasPackage: "#empty"
  assert_success
  assert_output "false"

  run_pharo loadHermes $ROOT_DIR/exported-empty-package.hermes --save
  assert_success

  run_pharo eval PackageOrganizer default hasPackage: "#empty"
  assert_success
  assert_output "true"
}

@test "loadHermes do not save if not specified" {
  copy_image "test-hermes.image"

  run_pharo eval PackageOrganizer default hasPackage: "#empty"
  assert_success
  assert_output "false"

  run_pharo loadHermes $ROOT_DIR/exported-empty-package.hermes
  assert_success

  run_pharo eval PackageOrganizer default hasPackage: "#empty"
  assert_success
  assert_output "false"
}

@test "loadHermes outputs error if no hermes file provided" {
  run_pharo loadHermes foo.bar
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*Missing Hermes file as argument[[:blank:]]*"
}