printing
printOn: aStream

	aStream nextPut: $[.
	aStream nextPutAll: self position printString; space.
	aStream nextPutAll: self type; space.
	aStream	 nextPutAll: self windowIndex printString.
	aStream nextPut: $].