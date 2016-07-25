testing
testRaiseDeprecationWarnings
	| case result |
	case := self class selector: #raiseDeprecationWarnings.
	result := case run.
	self assert: result defects asArray equals: (Array with: case).
	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 1
		errors: 0