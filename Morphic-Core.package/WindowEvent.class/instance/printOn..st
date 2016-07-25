printing
printOn: aStream

	aStream nextPut: $[.
	aStream nextPutAll: self type; space.
	aStream nextPut: $(; print: self rectangle; nextPut: $).
	aStream nextPut: $].