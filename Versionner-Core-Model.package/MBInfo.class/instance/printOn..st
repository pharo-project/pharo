printing
printOn: aStream
	self name ifNil: [ aStream nextPutAll: '' . ^ self ].
	aStream nextPutAll: self name