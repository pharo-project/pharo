tests - accessing
testAtColumn

	self assert: (matrix23 atColumn: 1) equals: #(11 12 13).
	self assert: (matrix23 atColumn: 2) equals: #(21 22 23).
	