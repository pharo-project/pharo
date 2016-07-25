reporting
reportVMStats: aStream
	self header: 'Virtual Machine Statistics' on: aStream.
	aStream
		nextPutAll: Smalltalk vm statisticsReport