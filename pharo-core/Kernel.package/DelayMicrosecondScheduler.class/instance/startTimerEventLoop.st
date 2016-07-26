timer process
startTimerEventLoop
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	
	timerEventLoop ifNotNil: [ self error: 'Timer event loop has already been started.' ].
	
	suspendedDelays := Heap 
			sortBlock: [ :delay1 :delay2 | delay1 schedulerResumptionTime <= delay2 schedulerResumptionTime ].
	timingSemaphore := Semaphore new.
	runTimerEventLoop := true.
	
	timerEventLoop := [ self runTimerEventLoop ] newProcess.
	timerEventLoop 
		name: 'Delay Scheduling Process';
		priority: Processor timingPriority.
	timerEventLoop resume.
	
	timingSemaphore signal. "get going"
