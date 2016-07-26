instance creation
forMilliseconds: aNumber
	"Return a new Delay for the given number of milliseconds. Sending 'wait' to this Delay will cause the sender's process to be suspended for approximately that length of time."

	^ self new setDelay: aNumber forSemaphore: Semaphore new
