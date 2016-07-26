printing
printOn: aStream
	aStream nextPutAll: 'NodeMorph('.
	complexContents printOn: aStream.
	aStream nextPut: $)