printing
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' code: '; print: color