printing
printOn: aStream
	"Append a sequence of characters to aStream that identify the receiver."

	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: left;
		space;
		print: right;
		nextPutAll: ')'