tests - accessing
testSwap
	matrix1 swap: 1 at: 2 with: 1 at: 1.
	self assert: (matrix1 at: 1 at: 1) = 3.
	self assert: (matrix1 at: 1 at: 2) = 1.