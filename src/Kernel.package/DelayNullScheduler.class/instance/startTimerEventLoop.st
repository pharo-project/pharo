timer process
startTimerEventLoop
	"Start a fake timer event loop that does nothing - just suspends via a wait that is never signalled"
	"Delay restartTimerEventLoop"
		
	timerEventLoop ifNotNil: [ self error: 'Timer event loop has already been started.' ].
		
	timerEventLoop := [ Processor activeProcess suspend. ] newProcess.
	timerEventLoop 
		name: 'WARNING! Delay Scheduling Process NOT RUNNING!';
		priority: Processor timingPriority.
	timerEventLoop resume.
