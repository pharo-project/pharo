printing
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' name: '; print: self name