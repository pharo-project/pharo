tests - termination
testIsTerminatingForcedTermination
	| process unwound started terminator unwindChecks terminationSemaphore |
	unwound := false.
	started := false.
	terminationSemaphore := Semaphore new.
	terminationSemaphore signal.
	process := [ 
		started := true.
		[ terminationSemaphore wait; wait. ] ensure: [
			terminator := Processor activeProcess.
			unwindChecks value.
			unwound := true ] 
	] newProcess.
	process priority: Processor activePriority - 1.
	
	self assert: process isSuspended.
	self deny: process isTerminating.
	self deny: process isTerminated.
	self deny: started.
	self deny: unwound.
	
	process resume.
	[ terminationSemaphore isSignaled ] whileTrue: [ 50 milliSeconds asDelay wait ].
	self deny: process isSuspended.
	self deny: process isTerminating.
	self deny: process isTerminated.
	self assert: started.
	self deny: unwound.
	self deny: terminator == process.
	
	terminationSemaphore signal.
	"This will run the #ensure: block and block on the Semaphore."
	process terminate.
	"We want these checks to run during unwinding, so we need to cheat a bit."
	unwindChecks := [
		self assert: process isSuspended.
		"We're inside of #terminate."
		self assert: process isTerminating.
		"Still unwinding, so not finished."
		self deny: process isTerminated.
		self assert: started.
		"Not unwound yet."
		self deny: unwound.
		self assert: terminator == process ].
	
	[ process isTerminated ] whileFalse: [ 50 milliSeconds asDelay wait ].
	self assert: process isSuspended.
	self assert: process isTerminating.
	self assert: process isTerminated.
	self assert: started.
	self assert: unwound.
	self assert: terminator == process