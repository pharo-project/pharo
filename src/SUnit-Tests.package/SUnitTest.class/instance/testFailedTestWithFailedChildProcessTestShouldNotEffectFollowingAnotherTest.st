testing
testFailedTestWithFailedChildProcessTestShouldNotEffectFollowingAnotherTest

	| result case1 case2 |
	DefaultExecutionEnvironment beActive.
	TestExecutionEnvironment new beActiveDuring: [ 
		case1 := self class selector: #failedTestWithFailedChildProcessTest.	
		result := case1 run.
		case2 := self class selector: #noop.	
		result := case2 run].

	self
		assertForTestResult: result
		runCount: 1
		passed: 1
		failed: 0
		errors: 0.
	self assertTerminatedFailedChildProcessesFor: case1