tests - some
testIntersection 
	| newFull col |
	full add: 3; add: 2.
	col := full intersection: full.
	self assert: (full = col).

	newFull := Set with: 8 with: 9 with: #z.
	col := newFull intersection: full.
	self assert: (col isEmpty).
	
	newFull add: 5; add: #abc; add: 7.
	col := newFull intersection: full.
	self assert: ((full select: [:each | (newFull includes: each)]) = col).
	
	
	