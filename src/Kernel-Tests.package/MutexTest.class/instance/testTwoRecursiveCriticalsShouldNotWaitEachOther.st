tests
testTwoRecursiveCriticalsShouldNotWaitEachOther

	| executed |
	executed := false.
	
	self fork: [mutex critical: [ mutex critical: [ executed := true ]]].
	self waitLastProcessTerminate.
		
	self assert: executed