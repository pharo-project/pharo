tests
testArithmetic

	| laterDate |
	laterDate := june2nd1973 addDays: 32.

	self 
		assert: (laterDate subtractDate: june2nd1973) equals: 32;
		assert: (june2nd1973 subtractDate: laterDate) equals: -32;
		assert: (laterDate subtractDays: 32) equals: june2nd1973.
