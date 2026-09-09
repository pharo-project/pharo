#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  kill-process-or-process-group
  teardown-workdir
}

@test "run --help prints help" {
  run_pharo run --help
  assert_output --partial "Usage: run [--help] [--list] [<applicationName>"
  assert_success
}

@test "run can list avaible applications that can be run" {
  copy_image "test-run.image"
  run_pharo eval --save "((SpApplication << #TestSpApplication) install) class compile: 'applicationName ^ #TestSpApp'"
  assert_success
  
  run_pharo run --list

  assert_success
  assert_line --index 0 --partial "List of applications:"
  assert_line "TestSpApp"
}

@test "run provides error message when no application name provided" {
  run_pharo run
  assert_failure
  assert_output --partial 'Please provide an application name to run'
}

@test "run provides error message when wrong application name provided" {
  run_pharo run foo
  assert_failure
  assert_output --partial 'Application foo not found.'
}

@test "can run a Spec application" {
  copy_image "test-run.image"
  run_pharo eval --save "| app | app := (SpApplication << #TestSpApplication) install. app class compile: 'applicationName ^ #TestSpApp'. app compile: 'start ''TestSpApp.running'' asFileReference ensureCreateFile'"
  assert_success
  refute_file_exists 'TestSpApp.running'
  
  run_pharo_in_backgroud run TestSpApp
  assert_success
  sleep 1

  assert_file_exists 'TestSpApp.running'
  rm -f 'TestSpApp.running'
}