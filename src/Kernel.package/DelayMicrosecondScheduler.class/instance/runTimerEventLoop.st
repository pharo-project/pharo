timer process
runTimerEventLoop
	| microsecondNowTick microsecondNextTick   |
	"Run the timer event loop."
	[ runTimerEventLoop ] whileTrue: [ 
		timingSemaphore wait.		
		microsecondNowTick := Time microsecondClockValue.
		microsecondNextTick := self handleTimerEvent: microsecondNowTick.

		"Since we have processed all outstanding requests, reset the timing semaphore so
		that only new work will wake us up again. Do this RIGHT BEFORE setting the next
		wakeup call from the VM because it is only signaled once so we mustn't miss it."
		timingSemaphore consumeAllSignals.
		self primSignal: timingSemaphore atUTCMicroseconds: microsecondNextTick.
		].
