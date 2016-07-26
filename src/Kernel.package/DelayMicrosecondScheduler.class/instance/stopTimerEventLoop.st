timer process
stopTimerEventLoop
	"Stop the timer event loop"
	|  suspendedDelayToClear |
	
	runTimerEventLoop := false.
	timingSemaphore signal. 
	
	"The signalled higher priority process should have exited the timer event loop 
	before the next line, but maybe not if that process was suspended, so force termination." 
	timerEventLoop ifNotNil: [ timerEventLoop terminate ]. 
	timerEventLoop := nil.

	"Expire any remaining delays."
	activeDelay ifNotNil: [ activeDelay schedulerSignalWaitingProcess ].
	[ suspendedDelayToClear := suspendedDelays removeFirstOrNil ]
		whileNotNil: [ suspendedDelayToClear schedulerSignalWaitingProcess ].
