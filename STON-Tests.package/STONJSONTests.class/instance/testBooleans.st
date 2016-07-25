tests
testBooleans
	self assert: (STONJSON toString: true) equals: 'true'.
	self assert: (STONJSON toString: false) equals: 'false'.
	self assert: (STONJSON fromString: 'true') equals: true.
	self assert: (STONJSON fromString: 'false') equals: false.