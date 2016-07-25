tests - conversion
testCeiling
	self assert: 1.0 ceiling = 1.
	self assert: 1.1 ceiling = 2.
	self assert: -2.0 ceiling = -2.
	self assert: -2.1 ceiling = -2.