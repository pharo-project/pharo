torevisit- tests - accessing
testAtColumn

	self assert: (grid23 atColumn: 1) equals: #(11 12 13).
	self assert: (grid23 atColumn: 2) equals: #(21 22 23).
	