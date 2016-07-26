tests
testSimpleCommunication
	"A process waiting on a signal will not proceed until one is sent"

	| semaphore wasBlockStarted wasBlockFinished |
	wasBlockStarted := wasBlockFinished := false.
	semaphore := Semaphore new.
	
	[
		wasBlockStarted := true.
		semaphore wait.
		wasBlockFinished := true ] fork.

	Processor yield.
	
	self assert: wasBlockStarted.
	self deny: wasBlockFinished.
	
	semaphore signal.
	Processor yield.
	
	self assert: wasBlockFinished.