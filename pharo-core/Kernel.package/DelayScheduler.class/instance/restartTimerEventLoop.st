timer process
restartTimerEventLoop
	"Start the timer event loop"
	"Delay restartTimerEventLoop"
	self stopTimerEventLoop.
	self startTimerEventLoop.

