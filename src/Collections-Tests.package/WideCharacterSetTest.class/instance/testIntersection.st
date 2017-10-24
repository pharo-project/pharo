some tests
testIntersection 
	| newFull col |
	full add: $y; add: $d.
	col := full intersection: full.
	self assert: col equals: full.

	newFull := self classToBeTested 
		with: (Character codePoint: 257) with: $B with: $C.
	col := newFull intersection: full.
	self assert: col equals: (Set with: (Character codePoint: 257)).
	newFull add: $x; add: $y; add: $z.
	col := newFull intersection: full.
	self assert: (full select: [:each | (newFull includes: each)]) equals: col
	