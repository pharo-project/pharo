printing
printOn: aStream
	
	super printOn: aStream.
	aStream
		nextPutAll: ' start '; print: start;
		nextPutAll: ' stop '; print: position;
		nextPutAll: ' max '; print: length.
