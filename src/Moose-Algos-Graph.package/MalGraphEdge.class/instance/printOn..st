accessing
printOn: aStream
	self from printOn: aStream.
	aStream << ' -> '.
	self to printOn: aStream