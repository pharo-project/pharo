accessing
objectPrintOn: aStream
	self id ifNil: [ ^ self ].
	
	aStream nextPutAll: self id hexString