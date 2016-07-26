ui
gtDisplayOn: aStream
	self example gtDisplayOn: aStream. 
	aStream nextPutAll: ' --> '.
	self unexpectedError 
		ifNotNil: [ self unexpectedError gtDisplayOn: aStream ]
		ifNil: [ self returnValue gtDisplayOn: aStream ].
