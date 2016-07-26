tests
testTwoCriticalsShouldWaitEachOther

	| lastExecuted lastExecutedAfterFirst |
	lastExecuted := false.
	lastExecutedAfterFirst := nil.
	
	self fork: [mutex critical: [ 5 milliSeconds wait.
			lastExecutedAfterFirst := lastExecuted not ]].
	
	self fork: [mutex critical: [ lastExecuted := true ]].
	10 milliSeconds wait.
		
	self assert: lastExecuted.
	self assert: lastExecutedAfterFirst 