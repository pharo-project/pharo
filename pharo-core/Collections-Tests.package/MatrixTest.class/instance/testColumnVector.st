tests - instance creation
testColumnVector

	| m m2 | 

	m := Matrix columnVector: #(1 2 3 4 5).

	self assert: m numberOfColumns equals: 1.
	self assert: m numberOfRows equals: 5.
	1 to: 5 do: [ :i |
		self assert: (m at: i at: 1) equals: i.].

	m2 := Matrix columnVector: Array new.

	self assert: m2 numberOfColumns equals: 1.
	self assert: m2 numberOfRows equals: 0.
	