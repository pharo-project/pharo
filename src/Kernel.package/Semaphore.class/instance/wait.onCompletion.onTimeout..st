communication
wait: aDuration onCompletion: completionBlock onTimeout: timeoutBlock
	"Wait on this semaphore for up to the given time duration, then timeout.
	If the deadline expired execute timeoutBlock, otherwise execute completionBlock.
	Return the value returned by the executed block."
	| d |
	d := DelayWaitTimeout new setDelay: aDuration asMilliSeconds forSemaphore: self.
	^d waitOnCompletion: completionBlock onTimeout: timeoutBlock