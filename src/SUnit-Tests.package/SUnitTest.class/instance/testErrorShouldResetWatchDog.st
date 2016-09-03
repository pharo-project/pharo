testing
testErrorShouldResetWatchDog

	| case result |
	
	DefaultExecutionEnvironment beActive.
	TestExecutionEnvironment new beActiveDuring: [ 
		case := self class selector: #fail.
		result := case run.
		1 seconds wait]. 

	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 1
		errors: 0