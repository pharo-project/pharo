printing
printOn: aStream

	self key printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream