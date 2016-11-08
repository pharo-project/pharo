testing
testTerminationShouldProceedEnsureBlocks

	| ensureCalled process |
	ensureCalled := false.
	process := [ 
		[10 seconds wait] ensure: [ ensureCalled := true ]
	] fork.
	Processor yield.
	
	process terminate.
	self assert: ensureCalled