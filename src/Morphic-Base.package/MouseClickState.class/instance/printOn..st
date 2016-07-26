printing
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $[; print: clickState; nextPut: $]
