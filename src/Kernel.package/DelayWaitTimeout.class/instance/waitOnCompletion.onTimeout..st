waiting
waitOnCompletion: completionBlock onTimeout: timeoutBlock
	"Wait until either the semaphore is signaled or the delay times out.
	If the delay times out execute timeoutBlock, otherwise if the semaphore is signaled execute completionBlock.
	Return the value returned by the executed block."
	^ self wait
		ifTrue: [ timeoutBlock value ]
		ifFalse: [ completionBlock value]