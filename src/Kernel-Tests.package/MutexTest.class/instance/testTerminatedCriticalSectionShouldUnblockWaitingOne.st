tests
testTerminatedCriticalSectionShouldUnblockWaitingOne
	| lastCriticalExecuted semaphoreToHoldMutex processHoldingMutex |
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	
	processHoldingMutex := self fork: [
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		self error: 'should not happen' ]].
	self waitLastProcessLock.
	
	self fork: [mutex critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.
	
	processHoldingMutex terminate.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted