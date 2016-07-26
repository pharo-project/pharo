printing
printOn: aStream

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self state;
		nextPut: $:;
		print: self duration;
		nextPut: $).

