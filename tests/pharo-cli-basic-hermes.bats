#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
  copy_image "test-hermes.image"
  # Ensure basic Hermes command line handler will be activated
  run_pharo perform --save HermesClapCommand removeFromSystem
}

teardown() {
  teardown-workdir
}

@test "loadHermes can load a hermes file and save before quitting" {
  run_pharo eval PackageOrganizer default hasPackage: "#empty"
  assert_success
  assert_output "false"

  run_pharo loadHermes $ROOT_DIR/exported-empty-package.hermes --save
  assert_success

  run_pharo eval PackageOrganizer default hasPackage: "#empty"
  assert_success
  assert_output "true"
}