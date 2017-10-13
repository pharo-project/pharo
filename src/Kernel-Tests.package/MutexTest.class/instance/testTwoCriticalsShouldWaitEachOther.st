tests
testTwoCriticalsShouldWaitEachOther

	| lastCriticalExecuted firstCriticalExecutedFirst semaphoreToHoldMutex |
	lastCriticalExecuted := false.
	firstCriticalExecutedFirst := false.
	semaphoreToHoldMutex := Semaphore new.
	
	self fork: [
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		firstCriticalExecutedFirst := lastCriticalExecuted not ]].
	self waitLastProcessLock.
	
	self fork: [mutex critical: [ lastCriticalExecuted := true ]].
	self waitLastProcessLock.
	
	semaphoreToHoldMutex signal.
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted.
	self assert: firstCriticalExecutedFirst 