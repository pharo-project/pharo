tests-perform
testPerformRenameInstanceVariable
	| change |
	change := changes renameInstanceVariable: 'instVar' to: 'instVarPlus' in: self changeMock.
	self perform: change do: [ 
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)