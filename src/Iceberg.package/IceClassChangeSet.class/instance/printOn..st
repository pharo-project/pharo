printing
printOn: aStream
	aStream 
		print: self class;
		nextPut: $(;
		nextPutAll: targetClass description;
		nextPut: $)
		