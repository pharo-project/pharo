tests - instance creation
testDiagonal

	| m m2 | 

	m := Matrix diagonal: #(1 2 3 4 5).

	self assert: m numberOfColumns equals: 5.
	self assert: m numberOfRows equals: 5.
	self assert: (m occurrencesOf: 0) equals: 20.
	1 to: 5 do: [ :i |
		self assert: (m at: i at: i) equals: i.].

	m2 := Matrix diagonal: Array new.

	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 0.
	