tests
testRenameInstanceVariable
	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class.
	self assert: change changeClassName = self class name.
	self assert: change oldName = 'instVar1'.
	self assert: change newName = 'instVar2'.
	self universalTestFor: change