mutual exclusion
critical: mutuallyExcludedBlock			
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in
	the process of running the critical: message. If the receiver is, evaluate
	mutuallyExcludedBlock after the other critical: message is finished."
	
	"We need to catch eventual interruptions very carefully.
	The naive approach of just doing, e.g.,:
			self wait.
			aBlock ensure: [self signal].
	will fail if the active process gets terminated while in the wait.
	However, the equally naive:
			[self wait.
			aBlock value] ensure: [self signal].
	will fail too, since the active process may get interrupted while
	entering the ensured block and leave the semaphore signaled twice.
	To avoid both problems we make use of the fact that interrupts only
	occur on sends (or backward jumps) and use an assignment (bytecode)
	right before we go into the wait primitive (which is not a real send and
	therefore not interruptable either)."
	
	| blockValue caught |
	caught := false.
	[
		caught := true.
		self wait.
		blockValue := mutuallyExcludedBlock value
	] ensure: [caught ifTrue: [self signal]].
	^blockValue
