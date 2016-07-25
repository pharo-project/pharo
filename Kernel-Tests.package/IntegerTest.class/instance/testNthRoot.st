tests - mathematical functions
testNthRoot
	1 << 2000 nthRoot: 100.
	self assert: (1 << 2000 nthRoot: 100) equals: 1 << 20