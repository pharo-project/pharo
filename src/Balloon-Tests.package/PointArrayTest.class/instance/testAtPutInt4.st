tests
testAtPutInt4

	pointArray at: 2 put: 405933 @ -405933.
	self assert: 405933@ -405933 equals: (pointArray at: 2)
	