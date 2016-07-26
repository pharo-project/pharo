tests
testFailedCriticalSectionShouldUnblockWaitingOne

	| lastExecuted |
	lastExecuted := false.
	
	self fork: [[mutex critical: [ 5 milliSeconds wait.
			self error: 'critical section failed' ]] ifError: []].
	
	self fork: [mutex critical: [ lastExecuted := true ]].
	10 milliSeconds wait.
		
	self assert: lastExecuted