controlling
runTestCase: aTestCase
	testCase := aTestCase.
	maxTimeForTest := testCase defaultTimeLimit.
	testCompleted := false.
	watchDogSemaphore signal. "signal about new test case"

	[[self runTestCaseSafelly: aTestCase] ensure: [
		testCompleted := true.
		watchDogSemaphore signal]. "signal that test case completes".
	
	self checkForkedProcesses] ifCurtailed: [
		forkedProcesses removeAll.
		failedProcesses removeAll]