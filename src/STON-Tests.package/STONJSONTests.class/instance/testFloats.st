tests
testFloats
	self assert: (STONJSON toString: 1.0) equals: '1.0'.
	self assert: (STONJSON toString: 0.0) equals: '0.0'.
	self assert: (STONJSON toString: -1.0) equals: '-1.0'.
	self assert: (STONJSON toString: -1.23456e-6) equals: '-1.23456e-6'.
	self assert: ((STONJSON fromString: '1.0') closeTo: 1.0).
	self assert: ((STONJSON fromString: '0.0') closeTo: 0.0).
	self assert: ((STONJSON fromString: '-1.0') closeTo: -1.0).
	self assert: ((STONJSON fromString: '-1.23456e-6') closeTo: -1.23456e-6).