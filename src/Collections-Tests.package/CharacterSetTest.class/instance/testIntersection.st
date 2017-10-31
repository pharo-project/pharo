tests
testIntersection 
	| newFull col |
	full add: $y; add: $d.
	col := full intersection: full.
	self assert: full equals: col.

	newFull := self classToBeTested with: $A with: $B with: $C.
	col := newFull intersection: full.
	self assert: (col isEmpty).
	
	newFull add: $x; add: $y; add: $z.
	col := newFull intersection: full.
	self assert: (full select: [:each | (newFull includes: each)]) equals: col
	