tests
assert: result equals: expected 
	result = expected
		ifFalse: [self signalFailure: 'Assertion failed: (' , result asString , ') ~= (' , expected asString , ')']