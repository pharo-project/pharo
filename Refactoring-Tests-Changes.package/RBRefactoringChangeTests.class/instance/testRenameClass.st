tests
testRenameClass
	| change |
	change := changes renameClass: self class to: self class name , 'Plus'.
	self assert: change oldName = self class name.
	self assert: change newName = (self class name , 'Plus').
	self assert: change changeClass = self class.
	self universalTestFor: change