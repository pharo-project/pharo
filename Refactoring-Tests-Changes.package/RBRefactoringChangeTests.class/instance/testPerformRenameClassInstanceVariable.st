tests-perform
testPerformRenameClassInstanceVariable
	| change |
	change := changes renameInstanceVariable: 'classInstVar' to: 'classInstVarPlus' in: self changeMock class.
	self perform: change do: [ 
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)