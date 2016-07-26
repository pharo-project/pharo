printing
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' (pane = ', self pane printString , ')'