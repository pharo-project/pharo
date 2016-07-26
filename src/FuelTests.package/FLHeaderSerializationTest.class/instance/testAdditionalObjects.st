tests
testAdditionalObjects

	| materialization |
	self serializer at: #test putAdditionalObject: 'test'.
	self serializer at: 42 putAdditionalObject: 68.
	
	self assertSerializationEqualityOf: 'foo'.

	self assert: (self materialization additionalObjectAt: #test) equals: 'test'.
	self assert: (self materialization additionalObjectAt: 42) equals: 68.
	