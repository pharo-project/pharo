Extension { #name : 'Semaphore' }

{ #category : '*System-Time' }
Semaphore >> wait: aDuration [
	"Wait on this semaphore for up to the given time duration, then timeout.
	Return true if the deadline expired, false otherwise."

	self deprecated: 'Use #waitTimeoutMilliseconds: instead'.
	^ self waitTimeoutMilliseconds: aDuration asMilliSeconds
]

{ #category : '*System-Time' }
Semaphore >> wait: aDuration onCompletion: completionBlock onTimeout: timeoutBlock [
	"Wait on this semaphore for up to the given time duration, then timeout.
	If the deadline expired execute timeoutBlock, otherwise execute completionBlock.
	Return the value returned by the executed block."

	self deprecated: 'Use #waitTimeoutMilliseconds:onCompletion:onTimeout: instead'.
	^ self
		  waitTimeoutMilliseconds: aDuration asMilliSeconds
		  onCompletion: completionBlock
		  onTimeout: timeoutBlock
]

{ #category : '*System-Time' }
Semaphore >> waitTimeoutMSecs: anInteger [

	self
		deprecated: 'Use #waitTimeoutMilliseconds: instead'
		transformWith:
		'`@x waitTimeoutMSecs: `@y' -> '`@x waitTimeoutMilliseconds: `@y'.
	^ self waitTimeoutMilliseconds: anInteger
]
