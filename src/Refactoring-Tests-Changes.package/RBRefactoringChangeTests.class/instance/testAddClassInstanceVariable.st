tests
testAddClassInstanceVariable
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class class.
	self assert: change isMeta.
	self assert: change variable = 'instVar'.
	self universalTestFor: change