tests - instance creation
testCreationGridCreationWithColumnsRows
	self assert: (grid62 atRow: 2 atColumn: 6) equals: 62.
	self assert: (grid62 atRow: 1 atColumn: 6) equals: 61.
	