tests - termination
testIsTerminatingForcedTerminationWithoutRunning
	| process unwound started terminator |
	unwound := false.
	started := false.
	process := [ 
		started := true.
		[ Semaphore new wait ] ensure: [
			terminator := Processor activeProcess.
			unwound := true ] 
	] newProcess.
	"This will prevent the process from getting a chance to run.
	Effectively the pc of suspendedContext will be equal to startpc."
	process priority: Processor systemBackgroundPriority.
	
	self assert: process isSuspended.
	self deny: process isTerminating.
	self deny: process isTerminated.
	self deny: started.
	self deny: unwound.
	
	process resume.
	self deny: process isSuspended.
	self deny: process isTerminating.
	self deny: process isTerminated.
	self deny: unwound.
	
	process terminate.
	self assert: process isSuspended.
	self assert: process isTerminating.
	self assert: process isTerminated.
	"The process never ran"
	self deny: started.
	"No unwind blocks were ever activated, so the next two lines can't be true."
	self deny: unwound.
	self deny: terminator == process