tests
testTerminationDuringUnwind
	"An illustration of the issue of process termination during unwind.
	This uses a well-behaved unwind block that we should allow to complete
	if at all possible."
	"self debug: #testTerminationDuringUnwind"
	
	| unwindStarted unwindFinished p |
	unwindStarted := unwindFinished := false.
	p := [[] ensure:
			[ unwindStarted := true.
			Processor yield.
			unwindFinished := true. ]] fork.
	self deny: unwindStarted.
	Processor yield.
	self assert: unwindStarted.
	self deny: unwindFinished.
	p terminate.
	self assert: unwindFinished.