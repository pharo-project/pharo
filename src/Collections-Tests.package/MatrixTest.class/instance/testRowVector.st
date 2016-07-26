tests - instance creation
testRowVector

	| m m2 | 

	m := Matrix rowVector: #(1 2 3 4 5).

	self assert: m numberOfColumns equals: 5.
	self assert: m numberOfRows equals: 1.
	1 to: 5 do: [ :i |
		self assert: (m at: 1 at: i) equals: i.].

	m2 := Matrix rowVector: Array new.

	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 1.
	