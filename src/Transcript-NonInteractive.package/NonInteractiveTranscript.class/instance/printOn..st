printing
printOn: aStream
	self == Transcript ifFalse: [ ^ super printOn: aStream ].
	aStream nextPutAll: 'Transcript'