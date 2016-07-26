tests - instance creation
testNewTabulate

	| m m2 | 

	m := Matrix new: 5 tabulate: [ :a :b | a*b ].

	self assert: m numberOfColumns equals: 5.
	self assert: m numberOfRows equals: 5.
	self assert: (m occurrencesOf: 8) equals: 2.
	1 to: 5 do: [ :i |
		self assert: (m at: i at: 1) equals: i.
		self assert: (m at: 1 at: i) equals: i.
		self assert: (m at: i at: i) equals: i*i.].

	m2 := Matrix new: 0 tabulate: [ :a :b | a*b ].
	
	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 0.
	self assert: (m2 occurrencesOf: 1) equals: 0.
	