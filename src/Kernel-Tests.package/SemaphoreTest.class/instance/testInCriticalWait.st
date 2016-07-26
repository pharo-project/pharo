tests
testInCriticalWait	"self run: #testSemaInCriticalWait"
	"This tests whether a semaphore that has entered the wait in Semaphore>>critical:
	leaves it without signaling the associated semaphore."
	| s p |
	s := Semaphore new.
	p := [s critical:[]] fork.
	Processor yield.
	self assert: (p suspendingList == s).
	p terminate.
	self assert:(s instVarNamed: #excessSignals) = 0