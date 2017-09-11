tests
testTerminatedCriticalSectionShouldUnblockWaitingOne

	| lastExecuted firstCritical |
	lastExecuted := false.
	
	firstCritical := self fork: [mutex critical: [ 10 seconds wait.
			self error: 'should not happen' ]].
	
	self fork: [mutex critical: [ lastExecuted := true ]].
	10 milliSeconds wait.
	firstCritical terminate.
	10 milliSeconds wait.
	self assert: lastExecuted