printing
printOn: aStream
	"Print the receiver on a stream"

	aStream nextPut: $[.
	aStream nextPutAll: type; nextPutAll: ' '''.
	self printKeyStringOn: aStream.
	aStream nextPut: $'.
	aStream nextPut: $]