tests
testRemovePool
	| change |
	change := changes removePool: 'PoolDict' from: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'PoolDict'.
	self universalTestFor: change