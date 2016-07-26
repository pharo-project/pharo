printing
printOn: aStream
	super printOn: aStream.
	aStream space print: self status.