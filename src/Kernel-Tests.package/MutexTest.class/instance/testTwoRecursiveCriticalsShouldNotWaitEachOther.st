tests
testTwoRecursiveCriticalsShouldNotWaitEachOther

	| executed |
	executed := false.
	
	self fork: [mutex critical: [ mutex critical: [ executed := true ]]].
	10 milliSeconds wait.
		
	self assert: executed