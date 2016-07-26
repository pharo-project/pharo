tests
testTerminatingBlockedCriticalWhichWasSignalledButNotResumedYet

	|  secondCriticalExecuted  firstCriticalExecuted firstCritical secondCritical lastCriticalExecuted controllingProcess |
	firstCriticalExecuted := false.
	secondCriticalExecuted := false.
	lastCriticalExecuted := false.
	controllingProcess := self fork: [ 5 milliSeconds wait.
		controllingProcess suspend.
		self assert: firstCriticalExecuted not description: 'controlling process should be resumed when first critical is finished execution'.
		Processor yield.
		self assert: firstCriticalExecuted description: 'controlling process not ready to terminate second critical because first is not finished execution'.
		self assert: firstCritical isTerminated description: 'controlling process not ready to terminate second critical because first is not terminated'.
		self assert: secondCriticalExecuted not description: 'second critical should not be executed before attemp to terminate it manually'.
		secondCritical terminate.
	].
	10 milliSeconds wait.
	self assert: controllingProcess isSuspended description: 'controlling process should be suspended'.
	firstCritical := self fork: [ mutex critical: [ 5 milliSeconds wait.
			 controllingProcess resume. Processor yield. "make chance for controlling process to check that our scenario is in correct state"
			firstCriticalExecuted := true ]].	
	"for second critical we choose small priority. So it can't be resumed automatically by scheduler in our scenario."
	secondCritical := self fork: [mutex critical: [ secondCriticalExecuted := true ]] at: Processor activeProcess priority - 1. 
	15 milliSeconds wait. "here we are waiting completion of our scenario. 
	At the end second critical should be terminated in waiting point when mutex was already signalled but process was not resumed.
	Correct critical implementation should allow execution of new consequent criticals"
	self fork: [ mutex critical: [ lastCriticalExecuted := true ]].	
	15 milliSeconds wait.
	self assert: lastCriticalExecuted description: 'consequent last critical should be executed'