tests
testSchedulerTermination
	"self debug: #testSchedulerTermination"
	
	| process sema gotHere sema2 |
	gotHere := false.
	sema := Semaphore new.
	sema2 := Semaphore new.
	process := [
		sema signal.
		sema2 wait.
		"will be suspended here"
		gotHere := true. "e.g., we must *never* get here" ] forkAt: Processor activeProcess priority.
	sema wait. "until process gets scheduled"
 	process terminate.
	sema2 signal.
	Processor yield. "will give process a chance to continue and
horribly screw up"
	self assert: gotHere not.
