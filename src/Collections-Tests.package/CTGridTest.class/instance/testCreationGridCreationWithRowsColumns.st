tests - instance creation
testCreationGridCreationWithRowsColumns
	self assert: (CTGrid grid6x2CreatedWithRowsColumns atRow: 2 atColumn: 6) equals: 62.
	self assert: (CTGrid grid6x2CreatedWithRowsColumns atRow: 1 atColumn: 6) equals: 61.
	