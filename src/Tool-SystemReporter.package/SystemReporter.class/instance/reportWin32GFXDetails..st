reporting
reportWin32GFXDetails: aStream
	self header: 'Graphics Hardware Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10003)