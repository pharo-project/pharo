tests
testCompileInClass
	| change |
	change := changes compile: 'setUp' in: self class.
	self assert: change controller isNil.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self assert: change source = 'setUp'.
	self assert: change protocol = #running.
	self universalTestFor: change