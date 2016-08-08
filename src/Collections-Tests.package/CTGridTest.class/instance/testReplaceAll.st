torevisit- tests - accessing
testReplaceAll

	grid1 replaceAll: 1 with: 10.
	self assert: (grid1 atRow: 1 atColumn: 1) equals: 10.
	self assert: (grid1 atRow: 2 atColumn: 1) equals: 2.
	self assert: (grid1 atRow: 1 atColumn: 2) equals: 3.
	self assert: (grid1 atRow: 2 atColumn: 2) equals: 4.