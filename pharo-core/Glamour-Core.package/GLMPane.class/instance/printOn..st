printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self identityHash printString;
		space;
		nextPutAll: self name;
		nextPut: $)