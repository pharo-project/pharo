protocol
atNewIndex: index put: anObject
	
	value atNewIndex: index put: anObject.
	self valueChanged: anObject.