Extension { #name : 'Semaphore' }

{ #category : '*Deprecated12' }
Semaphore >> waitTimeoutMSecs: anInteger [

	self
		deprecated: 'Use #waitTimeoutMilliseconds: instead'
		transformWith:
		'`@x waitTimeoutMSecs: `@y' -> '`@x waitTimeoutMilliseconds: `@y'.
	^ self waitTimeoutMilliseconds: anInteger
]
