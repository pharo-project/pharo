printing
printOn: aStream 
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self formattedCode;
		nextPut: $)