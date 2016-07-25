reporting
reportWin32OSDetails: aStream
	self header: 'Operating System Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10002)