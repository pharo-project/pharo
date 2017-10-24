tests
testPutAllNegative
	| array |
	array := IntegerArray new: 2.
	array  atAllPut: -1000.
	self assert: (array at: 2) = -1000