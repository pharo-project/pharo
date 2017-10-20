tests - termination
testTerminationShouldProceedEnsureBlocks

	| ensureCalled process semaphore |
	ensureCalled := false.
	semaphore := Semaphore new.
	process := [ 
		[semaphore signal. [10 milliSeconds wait] repeat] ensure: [ ensureCalled := true ]
	] fork.
	semaphore wait.
	
	process terminate.
	self assert: ensureCalled