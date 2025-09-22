#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "test --help prints help" {
  run_pharo test --help
  assert_output --partial "Usage: test [--help] [--junit-xml-output] [--shuffle-seed <shuffle-seed-value>] [--stage-name <stage-name-value>] [--no-xterm] [--fail-on-error] [--fail-on-failure] [--save] [--rename] [<PACKAGE>]"
  assert_success
}

@test "test can run tests on a package" {
  run_pharo test JenkinsTools-Tests
  assert_success
  assert_line "Finished running 16 Tests"
  assert_line --partial "Finished to run tests of JenkinsTools-Tests in"
  assert_line "16 run, 16 passes, 0 failures, 0 errors."
}

@test "test can run tests on a regex" {
  run_pharo test JenkinsTools.*
  assert_success
  assert_line "Running tests in 3 Packages"
  assert_line "Finished running 16 Tests"
  assert_line --partial "Finished to run tests of JenkinsTools-Tests in"
  assert_line "16 run, 16 passes, 0 failures, 0 errors."
}

@test "test can run tests with junit XML output" {
  run_pharo test --junit-xml-output JenkinsTools-Tests
  assert_success
  assert_line --partial "Finished to run tests of JenkinsTools-Tests"
  assert_file_exists "JenkinsTools-Tests-Test.xml"
}

@test "test runner fails if a test fails with flag --fail-on-failure" {
  copy_image "test-test-runner.image"
  run_pharo eval --save "ClapTestRunnerTest compile: 'testThatFails self assert: 1 equals: 0'"
  assert_success

  run_pharo test --fail-on-failure JenkinsTools-Tests
  assert_failure
  assert_line --partial "17 run, 16 passes, 1 failures, 0 errors."
}

@test "test runner does not fail if a test fails with flag --fail-on-error" {
  copy_image "test-test-runner.image"
  run_pharo eval --save "ClapTestRunnerTest compile: 'testThatFails self assert: 1 equals: 0'"
  assert_success

  run_pharo test --fail-on-error JenkinsTools-Tests
  assert_success
  assert_line --partial "17 run, 16 passes, 1 failures, 0 errors."
}

@test "test runner fails if a test fails with error with flag --fail-on-error" {
  copy_image "test-test-runner.image"
  run_pharo eval --save "ClapTestRunnerTest compile: 'testThatFailsWithError 1 / 0'"
  assert_success

  run_pharo test --fail-on-error JenkinsTools-Tests
  assert_failure
  assert_line --partial "17 run, 16 passes, 0 failures, 1 errors."
}