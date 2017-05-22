printing
printOn: aStream
	self isForced ifTrue: [ aStream nextPut: $+ ].
	aStream
		nextPutAll: self source;
		nextPut: $:;
		nextPutAll: self destination