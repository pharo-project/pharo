initialize - release
initializeWith: fileIDsArray readBlocking: aBoolean
	super initialize.
	readBlocking := aBoolean.
	self openReadStreamFor: fileIDsArray first withName: 'pipeReader'.
	self openWriteStreamFor: fileIDsArray second withName: 'pipeWriter'.
	