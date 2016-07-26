tests
testIsZero
	self assert: Duration zero isZero.
	self assert: (Duration seconds: 0) isZero	