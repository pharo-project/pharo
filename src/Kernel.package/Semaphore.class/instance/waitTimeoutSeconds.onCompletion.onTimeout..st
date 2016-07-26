communication
waitTimeoutSeconds: anInteger onCompletion: completionBlock onTimeout: timeoutBlock
	"Wait on this semaphore for up to the given number of seconds, then timeout.
	If the deadline expired execute timeoutBlock, otherwise execute completionBlock.
	Return the value returned by the executed block."
	| d |
	d := DelayWaitTimeout new setDelay: (anInteger * 1000 max: 0) forSemaphore: self.
	^d waitOnCompletion: completionBlock onTimeout: timeoutBlock