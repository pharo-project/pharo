tests
testAtPutFraction

	pointArray at: 2 put: 1/2 @ 51/5.
	self assert: 0 @ 10 equals: (pointArray at: 2)
	