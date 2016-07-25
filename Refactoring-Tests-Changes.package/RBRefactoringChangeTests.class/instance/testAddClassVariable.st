tests
testAddClassVariable
	| change |
	change := changes addClassVariable: 'ClassVar' to: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'ClassVar'.
	self universalTestFor: change