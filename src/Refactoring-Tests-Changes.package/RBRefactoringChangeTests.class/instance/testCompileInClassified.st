tests
testCompileInClassified
	| change |
	change := changes compile: 'setUp' in: self class classified: #accessing.
	self assert: change controller isNil.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self assert: change source = 'setUp'.
	self assert: change protocol = #accessing.
	self universalTestFor: change