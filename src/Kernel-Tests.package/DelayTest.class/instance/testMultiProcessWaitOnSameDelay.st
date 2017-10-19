tests - limits
testMultiProcessWaitOnSameDelay
	"Ensure that waiting on the same delay from multiple processes raises an error"
	| delay p1 p2 wasRun |
	delay := Delay forSeconds: 1.
	wasRun := false.
	p1 := [delay wait] forkAt: Processor activePriority+1.
	p2 := [
		self should:[delay wait] raise: Error.
		wasRun := true.
	] forkAt: Processor activePriority+1.
	p1 terminate.
	p2 terminate.
	self assert: wasRun.

