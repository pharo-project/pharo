printing
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' "'.
	aStream nextPutAll: contents.
	aStream nextPutAll: '" '.