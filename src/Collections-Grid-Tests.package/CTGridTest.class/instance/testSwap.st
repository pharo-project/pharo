tests - filling
testSwap

	grid62 swapAtRow: 1 atColumn: 2 withAtRow: 2 atColumn: 6.
	self assert: (grid62 atRow: 1 atColumn: 2) equals: 62.
	self assert: (grid62 atRow: 2 atColumn: 6) equals: 21.