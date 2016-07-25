timer process
stopTimerEventLoop
	"Stop the timer event loop"

	timerEventLoop ifNotNil: [ timerEventLoop terminate ]. 
	timerEventLoop := nil.

