printing
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self info name.
	aStream nextPut: $).