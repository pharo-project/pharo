tests
testTerminatingBlockedCriticalWhichWasSignalledButNotResumedYet
	| processWaitingForMutex firstCriticalExecuted lastCriticalExecuted semaphoreToHoldMutex |
	firstCriticalExecuted := false.
	lastCriticalExecuted := false.
	semaphoreToHoldMutex := Semaphore new.
	
	self fork: [ 
		mutex critical: [ semaphoreToHoldMutex wait.
		firstCriticalExecuted := true ]] at: Processor activeProcess priority + 1.	
	self waitLastProcessLock.
	"for second critical we choose small priority. So it can't be resumed automatically by scheduler in our scenario."
	processWaitingForMutex := self fork: [mutex critical: [ self error: 'should not happen' ]] at: Processor activeProcess priority - 1. 
	self waitLastProcessLock.
	self deny: firstCriticalExecuted.
	semaphoreToHoldMutex signal.
	self assert: firstCriticalExecuted.
	processWaitingForMutex terminate. "Here the process waits for mutex and being terminated at the point when mutex was already signalled but process was not resumed.
		Correct critical implementation should allow execution of new consequent criticals"
	self fork: [ mutex critical: [ lastCriticalExecuted := true ]].	
	self waitLastProcessTerminate.
	self assert: lastCriticalExecuted description: 'consequent last critical should be executed'