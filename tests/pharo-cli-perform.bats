#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "perform --help prints help" {
  run_pharo perform --help
  assert_success
  assert_output --partial "perform <global> <messageSelector>  [ <arguments> ] [ --save ]"
}

@test "perform can perform selector on class and quit" {
  run_pharo perform String name 
  assert_success
  assert_output "#String"
}

@test "perform can perform selector on class with arguments and quit" {
  run_pharo  perform Array with:with: 1 2
  assert_success
  assert_output "#('1' '2')"
}

@test "perform outputs error if receiver not found" {
  run_pharo perform Foo bar
  assert_failure
  assert_output --regexp "[[:blank:]]*Unknown class[[:blank:]]*"
}

@test "perform outputs error if selector not found" {
  run_pharo perform String bar
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*Instance of String class did not understand #bar[[:blank:]]*"
}

@test "perform does not save if flag --save absent" {
  copy_image "test-save.image"
  run_pharo perform ClapPharoCommandsTest testValue: Wednesday.
  assert_success

  run_pharo perform ClapPharoCommandsTest testValue
  assert_success
  refute_output --partial "Wednesday"
}

@test "perform --save preserves changes in the image" {
  copy_image "test-save.image"
  run_pharo perform --save ClapPharoCommandsTest testValue: Wednesday.
  assert_success

  # Check that the evaluation of the previous script has been persisted
  run_pharo perform ClapPharoCommandsTest testValue
  assert_success
  assert_output --partial "Wednesday"
}


