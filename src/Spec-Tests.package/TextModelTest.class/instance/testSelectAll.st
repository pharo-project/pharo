tests
testSelectAll
	self initText.
	self openInstance.
	testedInstance selectAll.
	self assert: testedInstance getSelection equals: (1 to: 15).
	testedInstance clearSelection.
	self assert: testedInstance getSelection isEmpty