controlling
runTestCaseSafelly: aTestCase
	[
		[aTestCase runCase] on: Halt do: [ :halt | 
			"if test was halted we should resume all background failures 
			to debug all of them together with test process"
			failedProcesses keysDo: #resume. halt pass ]
	] on: Error, TestFailure do: [ :err | 
		"error here means that test is failed. So we should check forked processes 
		and be able to debug all background failures together with original error"
		self checkForkedProcesses. 
		err pass  ]