#!/usr/bin/env bats

load ../test_helper.bash

setup() {
  setup-workdir
  copy_image "pharo-cli-perform-tests.image"
  # activate specified command line handler
  run_pharo perform --save $COMMAND_LINE_HANDLER initialize
  assert_success
}


teardown() {
  teardown-workdir
}

@test "perform --help prints help [$COMMAND_LINE_HANDLER]" {
  run_pharo perform --help
  assert_success
  assert_output --partial "Performs a message on a given class or global object."
  assert_output --partial "Usage: perform [--help] [--save] [--no-quit]"
}

@test "perform can perform selector on class and quit [$COMMAND_LINE_HANDLER]" {
  run_pharo perform String name 
  assert_success
  assert_output "#String"
}

@test "perform can perform selector on class with arguments and quit [$COMMAND_LINE_HANDLER]" {
  run_pharo perform Array with:with: 1 2
  assert_success
  assert_output "#('1' '2')"
}

@test "perform outputs error if receiver not found [$COMMAND_LINE_HANDLER]" {
  run_pharo perform Foo bar
  assert_failure
  assert_output --regexp "[[:blank:]]*Unknown class[[:blank:]]*"
}

@test "perform outputs error if selector not found [$COMMAND_LINE_HANDLER]" {
  run_pharo perform String bar
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*Instance of String class did not understand #bar[[:blank:]]*"
}

@test "perform does not save if flag --save absent [$COMMAND_LINE_HANDLER]" {
  run_pharo perform ClapPharoCommandsTest testValue: Wednesday
  assert_success

  run_pharo perform ClapPharoCommandsTest testValue
  assert_success
  refute_output --partial "Wednesday"
}

@test "perform --save preserves changes in the image [$COMMAND_LINE_HANDLER]" {
  run_pharo perform --save ClapPharoCommandsTest testValue: Wednesday
  assert_success

  # Check that the evaluation of the previous script has been persisted
  run_pharo perform ClapPharoCommandsTest testValue
  assert_success
  assert_output --partial "Wednesday"
}