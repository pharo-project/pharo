tests - termination
testIsTerminatingNormalTermination
	| sem process unwound started terminator |
	sem := Semaphore new.
	unwound := false.
	started := false.
	process := [ 
		started := true.
		[ sem wait ] ensure: [
		terminator := Processor activeProcess.
		unwound := true ] 
	] fork.
	self deny: process isSuspended.
	self deny: process isTerminating.
	self deny: process isTerminated.
	self deny: started.
	self deny: unwound.
	
	sem signal.
	self waitForProcessTermination: process.
	"#terminate will be sent by the process itself after its context has finished (see BlockClosure>>newProcess)"
	self assert: process isSuspended.
	self assert: process isTerminating.
	self assert: process isTerminated.
	self assert: started.
	"When inside of an unwind context, that unwind context needs run through."
	self assert: unwound.
	"A process should terminte itself."
	self assert: terminator == process