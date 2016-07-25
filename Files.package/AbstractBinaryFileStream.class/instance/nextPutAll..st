writing
nextPutAll: aByteArray

	forWrite ifFalse: [^ self error: 'Cannot write a read-only file'].
	
	self primWrite: handle from: aByteArray startingAt: 1 count: aByteArray basicSize.