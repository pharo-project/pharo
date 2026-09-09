#!/usr/bin/env bats

load test_helper.bash

setup() {
  setup-workdir
}

teardown() {
  kill-process-or-process-group
  teardown-workdir
}

@test "eval --help prints help" {
  run_pharo eval --help
  assert_success
  assert_output --partial "Usage: evaluate [--help] [--save] [--keepAlive] [--no-quit] [--rename] [<EXPR>]"
  assert_line --partial  "--help      Prints this documentation"
}

# @test "eval without valid Smalltalk expression exits with error" {
#   run_pharo eval
#   assert_success
#   assert_line --index 0 "Usage: eval [--help] [--save] [ --no-quit ] <smalltalk expression>"
#   assert_line --index 1 --regexp "--help[[:blank:]]+list this help message"
# }

@test "eval can evaluate a valid Smalltalk expression and quit" {
  run_pharo eval 3 + 5
  assert_success
  assert_output "8"
}

@test "eval can evaluate a valid Smalltalk expression from a file and quit" {
  echo "Transcript show: 'Hello World!'; cr" > "$WORKDIR/test-code.st"
  run_pharo eval "$WORKDIR/test-code.st"
  assert_success
  assert_line "Hello World!"
}

@test "eval outputs error if any when evaluating a valid Smalltalk expression with runtime error" {
  run_pharo eval 1 / 0
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*ZeroDivide"
  assert_line --index 1 --regexp "[[:blank:]]*SmallInteger>>/"
}

@test "eval outputs error if any when evaluating an invalid Smalltalk expression" {
  run_pharo eval 1 := .
  assert_failure
  assert_line --index 0 --regexp "[[:blank:]]*Syntax Error on line 1: 'End of statement expected'"
  assert_line --index 2 --regexp "[[:blank:]]*1: 1 := ."
  assert_line --index 3 --partial "_^_"
}

@test "eval with --no-quit keeps the image alive" {
  run_pharo_in_backgroud eval --no-quit 42
  # Give it a moment to start
  sleep 2

  assert_is_running $pid
}

@test "eval does not save if flag --save absent" {
  copy_image "test-save.image"
  run_pharo eval "Smalltalk at: #Point put: 123."
  assert_success

  run_pharo eval "Smalltalk at: #Point"
  assert_success
  refute_line "123"
}

@test "eval --save preserves changes in the image" {
  copy_image "test-save.image"
  run_pharo eval --save "Smalltalk at: #MyGlobal put: 123."
  assert_success

  # Check that the evaluation of the previous script has been persisted
  run_pharo eval "Smalltalk at: #MyGlobal"
  assert_success
  assert_line "123"
}


