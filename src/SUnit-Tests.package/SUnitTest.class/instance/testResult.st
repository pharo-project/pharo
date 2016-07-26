testing
testResult

	| case result |

	case := self class selector: #noop.
	result := case run.

	self
		assertForTestResult: result
		runCount: 1
		passed: 1
		failed: 0
		errors: 0