tests-perform
testPerformRenameClassVariable
	| change |
	change := changes renameClassVariable: 'ClassVar' to: 'ClassVarPlus' in: self changeMock.
	self perform: change do: [ 
		self deny: (change changeClass classVarNames includes: change oldName).
		self assert: (change changeClass classVarNames includes: change newName) ].
	self assert: (change changeClass classVarNames includes: change oldName).
	self deny: (change changeClass classVarNames includes: change newName)