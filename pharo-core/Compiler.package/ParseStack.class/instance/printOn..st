printing
printOn: aStream
	
	super printOn: aStream.
	aStream nextPutAll: ' at '; print: position; nextPutAll: ' of '; print: length