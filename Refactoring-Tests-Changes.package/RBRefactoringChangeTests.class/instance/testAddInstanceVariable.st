tests
testAddInstanceVariable
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'instVar'.
	self universalTestFor: change