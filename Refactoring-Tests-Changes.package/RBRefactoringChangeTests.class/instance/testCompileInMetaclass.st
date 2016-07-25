tests
testCompileInMetaclass
	| change |
	change := changes compile: 'new' in: self class class.
	self assert: change controller isNil.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class class.
	self assert: change isMeta.
	self assert: change selector = #new.
	self assert: change source = 'new'.
	self assert: change protocol = #accessing.
	self universalTestFor: change