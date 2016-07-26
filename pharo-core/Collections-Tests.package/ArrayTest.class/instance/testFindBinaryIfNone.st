tests - find binary
testFindBinaryIfNone
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ] ifNone: [ #none ]) = 11.
	self should: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 8 - arg ] ifNone: [ #none ]) = #none