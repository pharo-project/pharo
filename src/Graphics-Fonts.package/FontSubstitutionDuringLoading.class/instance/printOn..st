printing
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(;
		nextPutAll: familyName;
		nextPut: $-;
		print: pixelSize;
		nextPut: $).