tests-perform
testPerformRenameClass
	| change |
	change := changes renameClass: self changeMock to: self changeMock name , 'Plus'.
	self perform: change do: [ 
		self deny: (Smalltalk hasClassNamed: change oldName).
		self assert: (Smalltalk hasClassNamed: change newName) ].
	self assert: (Smalltalk hasClassNamed: change oldName).
	self deny: (Smalltalk hasClassNamed: change newName)