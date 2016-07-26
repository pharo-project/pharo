printing
printOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPut: ${.
	self printAsIfCompiledOn: aStream.
	aStream nextPut: $}.