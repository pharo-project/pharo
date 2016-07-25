printing
printOn: aStream
	aStream nextPut: $[.
	aStream nextPutAll: self type asString.
	aStream nextPut: $|.
	aStream nextPutAll: self text.
	aStream nextPut: $].