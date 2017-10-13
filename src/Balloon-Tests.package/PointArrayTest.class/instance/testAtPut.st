tests
testAtPut

	pointArray at: 2 put: -1 @ -1.
	self assert: 0 @ 0 equals: (pointArray at: 1).
	self assert: -1 @ -1 equals: (pointArray at: 2)
	