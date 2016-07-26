reporting
reportWin32HardwareDetails: aStream
	self header: 'Hardware Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10001)