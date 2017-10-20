tests
testTerminatingBlockedCriticalSectionShouldNotUnblockAnotherWaitingSection
	| semaphoreToHoldMutex holdingCriticalExecutedFirst firstWaitingProcess lastCriticalExecuted |
	holdingCriticalExecutedFirst := false.
	semaphoreToHoldMutex := Semaphore new.
	lastCriticalExecuted := false.
	
	self fork: [
		mutex critical: [ semaphoreToHoldMutex wait. "here we grab mutex and control it with semaphore"
		holdingCriticalExecutedFirst := lastCriticalExecuted not ]].
	self waitLastProcessLock.
	
	firstWaitingProcess := self fork: [mutex critical: [ self error: 'should not happen' ]].
	self waitLastProcessLock.
	self fork: [mutex critical: [ lastCriticalExecuted := true]].
	self waitLastProcessLock.

	firstWaitingProcess terminate.
	self waitLastProcessLock. "check that last process is still waiting"
	semaphoreToHoldMutex signal. "here we resume first process execution"
	self waitLastProcessTerminate.
	self assert: holdingCriticalExecutedFirst.
	self assert: lastCriticalExecuted.