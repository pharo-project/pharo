tests - point functions
testCrossProduct

	self assert: (0 @ 0 crossProduct: 0 @ 0) = 0.
	self assert: (1 @ 0 crossProduct: 1 @ 1) = 1.
	self assert: (3 @ 2 crossProduct: 6 @ 4) = 0.
	self assert: (1 @ 2 crossProduct: 3 @ 4) = -2.