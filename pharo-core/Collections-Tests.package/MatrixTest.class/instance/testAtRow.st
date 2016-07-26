tests - accessing
testAtRow

	self assert: (matrix23 atRow: 1) equals: #(11 21).
	self assert: (matrix23 atRow: 2) equals: #(12 22).
	self assert: (matrix23 atRow: 3) equals: #(13 23).