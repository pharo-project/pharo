printing
printOn: aStream
	super printOn: aStream.
	pointerArity timesRepeat: [ aStream nextPut: $* ]