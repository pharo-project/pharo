#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  kill-process-or-process-group
  teardown-workdir
}

@test "st --help prints help" {
  run_pharo st --help
  assert_success
  assert_line --index 0 "Usage: st [--help] [ --quit ] <FILE>"
  assert_line --index 1 --partial "--help    list this help message"
}

# @test "st without st file exits with error" {
#   run_pharo st --quit
#   assert_success
#   assert_line --index 0 "Usage: st [--help] [ --quit ] <FILE>"
#   assert_line --index 1 --partial "--help    list this help message"
# }

@test "st can load a valid st file and quit" {
  echo 'Transcript show: 3 + 5; cr' > "$WORKDIR/test-code.st"
  run_pharo st "$WORKDIR/test-code.st" --quit
  assert_success
  assert_output "8"
}

@test "st outputs error if any when loading st file" {
  echo '1 / 0' > "$WORKDIR/test-code.st"
  run_pharo st "$WORKDIR/test-code.st" --quit
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*ZeroDivide"
  assert_line --index 1 --regexp "[[:blank:]]*SmallInteger>>/"
}

@test "st without --quit keeps the image alive" {
  echo '42' > "$WORKDIR/long-running.st"
  run_pharo_in_backgroud  st "$WORKDIR/long-running.st"
  # Give it a moment to start
  sleep 2

  assert_is_running $pid
}

@test "st does not save if flag --save absent" {
  copy_image "test-save.image"
  echo "Smalltalk at: #CommandLineHandler put: 123." > "$WORKDIR/define-global.st"
  run_pharo st --quit "$WORKDIR/define-global.st"
  assert_success

  echo "Transcript show: (Smalltalk at: #CommandLineHandler) printString; cr." > "$WORKDIR/check-global.st"
  run_pharo st --quit "$WORKDIR/check-global.st"
  assert_success
  refute_line "123"
}

@test "st --save preserves changes in the image" {
  copy_image "test-save.image"
  echo "Smalltalk at: #MyGlobal put: 123." > "$WORKDIR/define-global.st"
  run_pharo st --quit --save "$WORKDIR/define-global.st"
  assert_success

  # Check that the evaluation of the previous script has been persisted
  echo "Transcript show: (Smalltalk at: #MyGlobal) printString; cr." > "$WORKDIR/check-global.st"
  run_pharo st "$WORKDIR/check-global.st" --quit
  assert_success
  assert_line "123"
}


