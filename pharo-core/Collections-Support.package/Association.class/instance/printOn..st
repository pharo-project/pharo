printing
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream