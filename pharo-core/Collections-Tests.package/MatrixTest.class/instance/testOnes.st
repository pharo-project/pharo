tests - instance creation
testOnes

	| m m2 | 

	m := Matrix ones: 10.

	self assert: m numberOfColumns equals: 10.
	self assert: m numberOfRows equals: 10.
	self assert: (m occurrencesOf: 1) equals: (10*10).

	m2 := Matrix ones: 0.

	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 0.
	self assert: (m2 occurrencesOf: 1) equals: 0.
	