tests - some
testUnion
	| newFull col newCol |
	full add: 3.
	full add: 2.
	col := full union: full.
	self assert: (full = col).

	newFull := Set with: 8 with: 9 with: #z.
	col := newFull union: full.
	self assert: (col size = (full size + newFull size)).
	self assert: ((col select: [:each | (full includes: each) not]) = newFull).
	self assert: ((col select: [:each | (newFull includes: each) not]) = full).

	full add: 9.
	col := newFull union: full.
	newCol := newFull copy.
	newCol remove: 9.
	self assert: (col size = (full size + newFull size - 1)).
	self assert: ((col select: [:each | (full includes: each) not]) = newCol).
	newCol := full copy.
	newCol remove: 9.
	self assert: ((col select: [:each | (newFull includes: each) not]) = newCol).
	
	
	