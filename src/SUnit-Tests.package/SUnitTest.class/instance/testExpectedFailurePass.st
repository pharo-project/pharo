testing
testExpectedFailurePass

	| case result |
	case := self class selector: #expectedFailurePasses.
	self deny: case shouldPass.
	result := case run.
	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 1
		errors: 0
		expectedFailures: 0.