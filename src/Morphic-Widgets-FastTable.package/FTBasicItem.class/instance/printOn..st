printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $[;
		nextPutAll: (self dataSource toString: data);
		nextPut: $]