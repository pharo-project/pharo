printing
printOn: aStream
	super printOn: aStream.
	aStream space; print: firstIndex; nextPutAll: ' to: '; print: lastIndex