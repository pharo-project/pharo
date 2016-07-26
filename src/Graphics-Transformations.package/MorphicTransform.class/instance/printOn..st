printing
printOn: aStream
	super printOn: aStream.
	aStream nextPut:$(;
		nextPutAll:'angle = '; print: angle;
		nextPutAll:'; scale = '; print: scale;
		nextPutAll:'; offset = '; print: offset;
		nextPut:$).