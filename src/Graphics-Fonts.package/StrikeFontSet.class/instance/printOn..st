printing
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: '(' , self name.
	aStream space.
	self height printOn: aStream.
	aStream nextPut: $).
