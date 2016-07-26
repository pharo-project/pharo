printing
printOn: aStream
	aStream nextPutAll: 'Node('.
	self item printOn: aStream.
	aStream nextPut: $)