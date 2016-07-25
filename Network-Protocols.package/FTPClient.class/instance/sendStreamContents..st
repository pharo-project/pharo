private
sendStreamContents: aStream
	self dataSocket sendStreamContents: aStream checkBlock: [self checkForPendingError. true]