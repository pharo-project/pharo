printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self name;
		space;
		print: self height;
		nextPut: $)