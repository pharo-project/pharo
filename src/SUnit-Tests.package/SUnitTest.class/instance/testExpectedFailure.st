testing
testExpectedFailure

	| case result |
	case := self class selector: #expectedFailureFails.
	self deny: case shouldPass.
	result := case run.
	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 0
		errors: 0
		expectedFailures: 1.