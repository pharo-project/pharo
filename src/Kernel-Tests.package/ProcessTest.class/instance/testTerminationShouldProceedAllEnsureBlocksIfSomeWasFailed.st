tests - termination
testTerminationShouldProceedAllEnsureBlocksIfSomeWasFailed

	| ensureCalled process ensureFailure forkedFailures |
	ensureFailure := Error new messageText: 'signalled inside ensure'.
	ensureCalled := false.
	process := [ 
		[[10 seconds wait] 
			ensure: [ ensureFailure signal ]]
				ensure: [ ensureCalled := true ].
	] fork.
	Processor yield.
	process terminate.	
	Processor yield.
	self assert: ensureCalled.
	forkedFailures := TestExecutionEnvironment currentFailures.
	self assert: forkedFailures size equals: 2.
	self assert: forkedFailures first equals: ensureFailure.
	self assert: forkedFailures last class equals: UnwindError.
	self assert: forkedFailures last signalerContext exception equals: ensureFailure.
	TestExecutionEnvironment resetFailures