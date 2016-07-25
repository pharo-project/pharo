tests
testWholeMicroseconds
	self assert: 0 seconds wholeMicroseconds equals: 0.
	self assert: 1 second wholeMicroseconds equals: 0.
	self assert: 1002003004 nanoSeconds wholeMicroseconds equals: 3