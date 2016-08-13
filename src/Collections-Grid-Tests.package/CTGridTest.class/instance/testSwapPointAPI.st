tests - filling
testSwapPointAPI
	grid62 swapAtPoint: (2@1) withPoint: (6@2).
	self assert: (grid62 atRow: 1 atColumn: 2) = 62.
	self assert: (grid62 atRow: 2 atColumn: 6) = 21.