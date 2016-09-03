controlling
activated

	| testProcess |
	testProcess := Processor activeProcess.
	watchDogSemaphore := Semaphore new.
	watchDogProcess := [self watchDogLoopFor: testProcess] newProcess.
	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchDogProcess 
		name: 'Tests execution watch dog';
		priority: Processor timingPriority-1;
		resume