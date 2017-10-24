tests
testAt

	self assert: 0@0 equals: (pointArray at: 1).
	self assert: 10@3 equals: (pointArray at: 2)
	