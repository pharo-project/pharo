tests
testNull
	self assert: (STONJSON toString: nil) equals: 'null'.
	self assert: (STONJSON fromString: 'null') equals: nil.