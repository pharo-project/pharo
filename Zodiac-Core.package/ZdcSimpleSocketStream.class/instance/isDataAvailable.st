testing
isDataAvailable
	"Return true when there is data available for reading.
	This does not block."
	
	readBuffer isEmpty ifFalse: [ ^ true ].
	(socket notNil and: [ self socketIsDataAvailable ]) 
		ifTrue: [
			"Assuming there really is data available, read it and recurse"
			self fillReadBufferNoWait.
			^ self isDataAvailable ].   
	^ false