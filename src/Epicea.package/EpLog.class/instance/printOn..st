printing
printOn: aStream

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: headReference asString;
		nextPut: $).