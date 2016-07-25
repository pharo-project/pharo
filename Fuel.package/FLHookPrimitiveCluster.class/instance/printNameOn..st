printing
printNameOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."
	
	super printNameOn: aStream.
	aStream nextPut: $[.
	theClass printOn: aStream.
	aStream nextPut: $].