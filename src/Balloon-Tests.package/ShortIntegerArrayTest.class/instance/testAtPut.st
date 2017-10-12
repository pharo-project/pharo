tests
testAtPut

	shortIntegerArray at: 3 put: 100.
	self assert: 100  equals: (shortIntegerArray at: 3)
	