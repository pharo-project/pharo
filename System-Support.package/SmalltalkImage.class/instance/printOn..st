printing
printOn: aStream
	self == Smalltalk ifFalse: [^super printOn: aStream].
	aStream nextPutAll: 'Smalltalk'