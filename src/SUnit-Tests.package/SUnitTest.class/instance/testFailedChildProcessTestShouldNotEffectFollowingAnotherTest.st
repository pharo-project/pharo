testing
testFailedChildProcessTestShouldNotEffectFollowingAnotherTest

	| case result |
	DefaultExecutionEnvironment beActive.
	TestExecutionEnvironment new beActiveDuring: [ 
		case := self class selector: #failedChildProcessTest.	
		result := case run.
		case := self class selector: #noop.	
		result := case run].

	self
		assertForTestResult: result
		runCount: 1
		passed: 1
		failed: 0
		errors: 0