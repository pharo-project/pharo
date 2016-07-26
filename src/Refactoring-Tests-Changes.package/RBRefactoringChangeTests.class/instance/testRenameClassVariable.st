tests
testRenameClassVariable
	| change |
	change := changes renameClassVariable: 'ClassVar1' to: 'ClassVar2' in: self class.
	self assert: change changeClassName = self class name.
	self assert: change oldName = 'ClassVar1'.
	self assert: change newName = 'ClassVar2'.
	self universalTestFor: change