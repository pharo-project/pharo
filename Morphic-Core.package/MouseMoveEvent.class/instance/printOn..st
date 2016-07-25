printing
printOn: aStream

	aStream nextPut: $[.
	aStream nextPutAll: self startPoint printString; space.
	aStream nextPutAll: self endPoint printString; space.
	aStream nextPutAll: self type; space.
	aStream nextPutAll: self modifierString.
	aStream nextPutAll: self buttonString.
	aStream nextPutAll: timeStamp printString; space.
	aStream	 nextPutAll: self windowIndex printString.
	aStream nextPut: $].