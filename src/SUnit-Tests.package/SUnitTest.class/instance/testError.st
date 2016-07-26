testing
testError

	| case result |

	case := self class selector: #error.
	result := case run.
	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 0
		errors: 1.

	case := self class selector: #errorShouldntRaise.
	result := case run.
	self 
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 0
		errors: 1