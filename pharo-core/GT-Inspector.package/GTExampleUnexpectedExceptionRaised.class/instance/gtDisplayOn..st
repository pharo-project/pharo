ui
gtDisplayOn: aStream
	super gtDisplayOn: aStream.
	aStream nextPutAll: ' ('.
	self unexpectedError gtDisplayOn: aStream.
	aStream nextPutAll: ')'.