tests
testIntegers
	self assert: (42 threeWayCompareTo: 42) equals: 0.
	self assert: (1 threeWayCompareTo: 42) equals: -1.
	self assert: (100 threeWayCompareTo: 42) equals: 1