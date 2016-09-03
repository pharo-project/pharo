testing
testLongRunningTestThenGreenTest

	| case result |
	DefaultExecutionEnvironment beActive.
	case := self class selector: #longRunningTest.
	result := case run.
	case := self class selector: #noop.
	result := case run.

	self
		assertForTestResult: result
		runCount: 1
		passed: 1
		failed: 0
		errors: 0