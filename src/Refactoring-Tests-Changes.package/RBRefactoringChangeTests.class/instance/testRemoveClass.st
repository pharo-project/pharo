tests
testRemoveClass
	| change |
	change := changes removeClass: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self universalTestFor: change