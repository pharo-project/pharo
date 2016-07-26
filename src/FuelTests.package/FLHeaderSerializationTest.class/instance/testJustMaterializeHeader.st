tests
testJustMaterializeHeader

	self serializer at: #test putAdditionalObject: 'test'.
	self serializer at: 42 putAdditionalObject: 68.
	
	self serialize: 'foo'.
	
	self assert: (self materializationHeader additionalObjectAt: #test) equals: 'test'.
	self assert: (self materializationHeader additionalObjectAt: 42) equals: 68.
	