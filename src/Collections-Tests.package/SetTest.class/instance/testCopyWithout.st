tests - some
testCopyWithout
	| newFull e |
	e := self elementNotIn.
	full add: e.
	newFull := full copyWithout: e.
	self assert: newFull size equals: full size - 1.
	self deny: (newFull includes: e).
	self assert: (newFull reject: [ :each | full includes: each ]) isEmpty.
	self
		assert: (full reject: [ :each | newFull includes: each ])
		equals: (self classToBeTested with: e)