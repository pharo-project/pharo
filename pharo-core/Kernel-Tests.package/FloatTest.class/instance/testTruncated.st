tests - conversion
testTruncated
	self assert: 1.0 truncated = 1.
	self assert: 1.1 truncated = 1.
	self assert: -2.0 truncated = -2.
	self assert: -2.1 truncated = -2.