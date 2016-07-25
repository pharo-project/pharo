tests
testAddPool
	| change |
	change := changes addPool: 'PoolDict' to: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'PoolDict'.
	self universalTestFor: change