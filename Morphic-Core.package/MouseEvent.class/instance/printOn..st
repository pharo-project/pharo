printing
printOn: aStream

	aStream nextPut: $[.
	aStream nextPutAll: self cursorPoint printString; space.
	aStream nextPutAll: type; space.
	aStream nextPutAll: self modifierString.
	aStream nextPutAll: self buttonString.
	aStream nextPutAll: timeStamp printString; space.
	aStream	 nextPutAll: self windowIndex printString.
	aStream nextPut: $].