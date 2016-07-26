tests
testRemoveMethod
	| change |
	change := changes removeMethod: #setUp from: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self universalTestFor: change