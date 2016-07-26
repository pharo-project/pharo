printing
storeOn: aStream
	"Store a description of the elements of the complement rather than self."
	
	aStream nextPut: $(.
	absent storeOn: aStream.
	aStream nextPut: $); space; nextPutAll: #complement.