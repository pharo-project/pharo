writing
writeSBITChunkOn: aStream
	"Write the IDAT chunk"
	aStream nextPutAll: 'sBIT' asByteArray.
	form depth = 16 ifFalse:[self error: 'Unimplemented feature'].
	aStream nextPut: 5.
	aStream nextPut: 5.
	aStream nextPut: 5.
	aStream nextPut: 1.