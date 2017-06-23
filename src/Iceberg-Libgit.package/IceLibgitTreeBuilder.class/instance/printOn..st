printing
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPut: $(.
	self printPathOn: aStream.
	aStream nextPut: $)
