printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: ' named ';
		nextPutAll: self name