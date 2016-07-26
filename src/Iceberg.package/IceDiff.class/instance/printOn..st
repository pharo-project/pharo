printing
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self repository location pathString;
		nextPut: $)