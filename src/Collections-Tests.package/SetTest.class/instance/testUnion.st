tests - some
testUnion
	| newFull col newCol |
	full add: 3.
	full add: 2.
	col := full union: full.
	self assert: full equals: col.
	newFull := self classToBeTested with: 8 with: 9 with: #z.
	col := newFull union: full.
	self assert: col size equals: full size + newFull size.
	self
		assert: (col reject: [ :each | full includes: each ])
		equals: newFull.
	self
		assert: (col reject: [ :each | newFull includes: each ])
		equals: full.
	full add: 9.
	col := newFull union: full.
	newCol := newFull copy.
	newCol remove: 9.
	self assert: col size equals: full size + newFull size - 1.
	self
		assert: (col reject: [ :each | full includes: each ])
		equals: newCol.
	newCol := full copy.
	newCol remove: 9.
	self
		assert: (col reject: [ :each | newFull includes: each ])
		equals: newCol