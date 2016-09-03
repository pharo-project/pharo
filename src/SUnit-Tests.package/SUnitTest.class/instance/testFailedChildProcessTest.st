testing
testFailedChildProcessTest

	| case result failedProcesses |
	DefaultExecutionEnvironment beActive.
	case := self class selector: #failedChildProcessTest.	
	result := case run.

	self
		assertForTestResult: result
		runCount: 1
		passed: 0
		failed: 0
		errors: 1.
		
	failedProcesses := Process allInstances 
		select: [: each | each name = #failedChildProcessTest].
	self assert: (failedProcesses allSatisfy: #isTerminated) 