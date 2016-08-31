communication
wait: aDuration
	"Wait on this semaphore for up to the given time duration, then timeout. 
	Return true if the deadline expired, false otherwise."
	| d |
	d := DelayWaitTimeout new setDelay: aDuration asMilliSeconds forSemaphore: self.
	^d wait