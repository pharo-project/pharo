printing
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: contents.
	aStream nextPut: $).