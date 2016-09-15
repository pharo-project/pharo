printing
printOn: aStream

	super printOn: aStream.
	
	aStream
		nextPut: $(;
		print: origin;
		nextPutAll: '->';
		print: target;
		nextPut: $).