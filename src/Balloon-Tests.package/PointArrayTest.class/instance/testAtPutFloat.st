tests
testAtPutFloat

	pointArray at: 2 put: 1.2 @ 5.5.
	self assert: 1 @ 5 equals: (pointArray at: 2)
	