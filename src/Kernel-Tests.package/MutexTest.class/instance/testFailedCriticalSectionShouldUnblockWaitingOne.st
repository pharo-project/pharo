tests
testFailedCriticalSectionShouldUnblockWaitingOne
	| lastCriticalExecuted semaphoreToHoldMutex processHoldingMutex |
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	
	processHoldingMutex := self fork: [[
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		self error: 'critical section failed' ]] ifError: []].
	self waitLastProcessLock. "wait until first process grabs the mutex"

	self fork: [mutex critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.
	
	semaphoreToHoldMutex signal.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted