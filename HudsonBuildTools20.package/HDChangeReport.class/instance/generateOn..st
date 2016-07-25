generating
generateOn: aStream
	aStream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; nextPut: Character lf.
	aStream nextPutAll: '<changelog>'; nextPut: Character lf.
	self resolved
		do: [ :each | self generateChangeSet: each on: aStream ].
	aStream nextPutAll: '<changelog>'