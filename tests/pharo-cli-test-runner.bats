#!/usr/bin/env bats

load test_helper.bash

BASE_TEST_COUNT=19
TOTAL_TEST_COUNT=20

setup() {
  setup-workdir
}

teardown() {
  teardown-workdir
}

@test "test --help prints help" {
  run_pharo test --help
  assert_output --partial "Usage: test [--help] [--junit-xml-output] [--shuffle-seed <shuffle-seed-value>] [--stage-name <stage-name-value>] [--project-name <project-name-value>] [--no-xterm] [--fail-on-error] [--fail-on-failure] [--save] [--rename] [<PACKAGE>]"
  assert_success
}

@test "test can run tests on a package" {
  run_pharo test SUnit-UI-Tests
  assert_success
  assert_line "Finished running ${BASE_TEST_COUNT} Tests"
  assert_line --partial "Finished to run tests of SUnit-UI-Tests in"
  assert_line "${BASE_TEST_COUNT} run, ${BASE_TEST_COUNT} passes, 0 failures, 0 errors."
}

@test "test can run tests on a regex" {
  run_pharo test SUnit-UI.*
  assert_success
  assert_line "Running tests in 2 Packages"
  assert_line "Finished running ${BASE_TEST_COUNT} Tests"
  assert_line --partial "Finished to run tests of SUnit-UI-Tests in"
  assert_line "${BASE_TEST_COUNT} run, ${BASE_TEST_COUNT} passes, 0 failures, 0 errors."
}

@test "test can run tests with junit XML output" {
  run_pharo test --junit-xml-output SUnit-UI-Tests
  assert_success
  assert_line --partial "Finished to run tests of SUnit-UI-Tests"
  assert_file_exists "SUnit-UI-Tests-Test.xml"
}

@test "test runner fails if a test fails with flag --fail-on-failure" {
  copy_image "test-test-runner.image"
  run_pharo eval --save "ClapTestRunnerTest compile: 'testThatFails self assert: 1 equals: 0'"
  assert_success

  run_pharo test --fail-on-failure SUnit-UI-Tests
  assert_failure
  assert_line --partial "${TOTAL_TEST_COUNT} run, ${BASE_TEST_COUNT} passes, 1 failures, 0 errors."
}

@test "test runner does not fail if a test fails with flag --fail-on-error" {
  copy_image "test-test-runner.image"
  run_pharo eval --save "ClapTestRunnerTest compile: 'testThatFails self assert: 1 equals: 0'"
  assert_success

  run_pharo test --fail-on-error SUnit-UI-Tests
  assert_success
  assert_line --partial "${TOTAL_TEST_COUNT} run, ${BASE_TEST_COUNT} passes, 1 failures, 0 errors."
}

@test "test runner fails if a test fails with error with flag --fail-on-error" {
  copy_image "test-test-runner.image"
  run_pharo eval --save "ClapTestRunnerTest compile: 'testThatFailsWithError 1 / 0'"
  assert_success

  run_pharo test --fail-on-error SUnit-UI-Tests
  assert_failure
  assert_line --partial "${TOTAL_TEST_COUNT} run, ${BASE_TEST_COUNT} passes, 0 failures, 1 errors."
}
