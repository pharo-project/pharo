tests
testRemoveClassInstanceVariable
	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class class.
	self assert: change isMeta.
	self assert: change variable = 'instVar'.
	self universalTestFor: change