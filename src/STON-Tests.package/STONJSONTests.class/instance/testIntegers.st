tests
testIntegers
	self assert: (STONJSON toString: 1) equals: '1'.
	self assert: (STONJSON toString: 0) equals: '0'.
	self assert: (STONJSON toString: -1) equals: '-1'.
	self assert: (STONJSON toString: 1234567890) equals: '1234567890'.
	self assert: (STONJSON fromString: '1') equals: 1.
	self assert: (STONJSON fromString: '0') equals: 0.
	self assert: (STONJSON fromString: '-1') equals: -1.
	self assert: (STONJSON fromString: '1234567890') equals: 1234567890.