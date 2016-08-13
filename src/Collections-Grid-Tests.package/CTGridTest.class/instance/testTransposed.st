torevisit- tests - accessing
testTransposed
	| transposed |
	transposed := grid1 transposed.
	self assert: (transposed atRow: 1 atColumn: 1) equals: 1.
	self assert: (transposed atRow: 1 atColumn: 2) equals: 2.
	self assert: (transposed atRow: 2 atColumn: 1) equals: 3.
	self assert: (transposed atRow: 2 atColumn: 2) equals: 4