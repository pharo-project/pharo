tests
testTerminatingBlockedCriticalSectionShouldNotUnblockAnotherWaitingSection

	| lastExecuted lastExecutedAfterFirst secondCritical |
	lastExecuted := false.
	lastExecutedAfterFirst := nil.
	
	self fork: [mutex critical: [ 10 milliSeconds wait.
			lastExecutedAfterFirst := lastExecuted not ]].
	
	secondCritical := self fork: [mutex critical: [ lastExecuted := true ]].
	self fork: [mutex critical: [ lastExecuted := true ]].
	5 milliSeconds wait.
	secondCritical terminate.
	10 milliSeconds wait.
	self assert: lastExecuted.
	self assert: lastExecutedAfterFirst