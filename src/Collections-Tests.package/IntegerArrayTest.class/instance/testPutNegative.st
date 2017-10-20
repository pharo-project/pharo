tests
testPutNegative
	| array |
	array := IntegerArray new: 2.
	array at: 2 put: -1000.
	self assert: (array at: 2) = -1000