tests
testRemoveClassNamed
	| change |
	change := changes removeClassNamed: self class name.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self universalTestFor: change