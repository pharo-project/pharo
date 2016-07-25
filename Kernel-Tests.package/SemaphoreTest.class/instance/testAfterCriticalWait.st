tests
testAfterCriticalWait	"self run: #testSemaAfterCriticalWait"
	"This tests whether a semaphore that has just left the wait in Semaphore>>critical:
	leaves it with signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical:[]] forkAt: Processor activePriority-1.
	"wait until p entered the critical section"
	[p suspendingList == s] whileFalse:[(Delay forMilliseconds: 10) wait].
	"Now that p entered it, signal the semaphore. p now 'owns' the semaphore
	but since we are running at higher priority than p it will not get to do
	anything."
	s signal.
	p terminate.
	self assert:(s instVarNamed: #excessSignals) = 1