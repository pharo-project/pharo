tests - find binary
testFindBinary
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 1 - arg ]) = 1.
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 23 - arg ]) = 23.
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]) = 11.
	self should: [ #(1 3 5 7 11 15 23) findBinary: [ :arg | 8 - arg ] ] raise: NotFound