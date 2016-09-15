testing
testChildProcessShouldNotBeStoredStronglyByTestEnvironment

	| process weak |
	process := [   ] forkNamed: 'child test process'.
	weak := WeakArray with: process.
	process := nil.
	Processor yield.
	Smalltalk garbageCollect.
	
	self assert: weak first equals: nil