printing
printOn: aStream
	aStream nextPutAll: 'ValueLink('.
	value printOn: aStream.
	aStream nextPut: $)
