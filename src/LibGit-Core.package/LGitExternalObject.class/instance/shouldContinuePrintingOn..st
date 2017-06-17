printing
shouldContinuePrintingOn: aStream
	self ifNotReady: [ aStream nextPutAll: ' (<not initialized>)' ].
	
	^ self isReady