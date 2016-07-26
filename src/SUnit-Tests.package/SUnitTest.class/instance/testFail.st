testing
testFail

	| case result |

	case := self class selector: #fail.
	result := case run.

	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 1
		errors: 0