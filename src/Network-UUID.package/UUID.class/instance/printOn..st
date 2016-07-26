printing
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'