tests - sunit original tests
testIllegal
	self should: [empty at: 5] raise: self defaultTestError.
	self should: [empty at: 5 put: #abc] raise: self defaultTestError.
			