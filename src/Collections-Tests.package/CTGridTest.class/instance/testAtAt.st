torevisit- tests - accessing
testAtAt
	self should:[grid1 atRow: 2 atColumn: 3] raise: Error.
	self should:[grid1 atRow: 3 atColumn: 2] raise: Error.
	self should:[grid1 atRow: 3 atColumn: 3] raise: Error.
	self should:[grid1 atRow: 0 atColumn: 1] raise: Error.
	self should:[grid1 atRow: 1 atColumn: 0] raise: Error.
	self should:[grid1 atRow: 0 atColumn: 0] raise: Error.
	
	self assert: (grid1 atRow: 1 atColumn: 1) equals: 1.
	self assert: (grid23 atRow: 1 atColumn: 1) equals: 11.
	self assert: (grid23 atRow: 1 atColumn: 2) equals: 21.
	self assert: (grid23 atRow: 2 atColumn: 1) equals: 12.
	self assert: (grid23 atRow: 2 atColumn: 2) equals: 22.
	self assert: (grid23 atRow: 3 atColumn: 1) equals: 13.
	self assert: (grid23 atRow: 3 atColumn: 2) equals: 23.