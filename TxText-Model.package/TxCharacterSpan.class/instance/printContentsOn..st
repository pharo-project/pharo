printing
printContentsOn: aStream
	aStream 
		nextPutAll: '"';
		nextPutAll: characters; 
		nextPutAll: '"'.
	