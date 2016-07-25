printing
printOn: aStream
	aStream nextPutAll: self class printString.
	self printTestSelectorOn: aStream