tests
testSchedulesFIFO
	"Based only on wait time, independent of process priority"

	| semaphore waitingLongestResumed higherPriorityResumed |
	waitingLongestResumed := higherPriorityResumed := false.
	semaphore := Semaphore new.
	
	[
		semaphore wait.
		waitingLongestResumed := true ] forkAt: Processor activeProcess priority + 10.
	[
		semaphore wait.
		higherPriorityResumed := true ] forkAt: Processor activeProcess priority + 20.

	self deny: waitingLongestResumed.
	self deny: higherPriorityResumed.

	semaphore signal.
	self assert: waitingLongestResumed.
	
	semaphore signal.
	self assert: higherPriorityResumed.