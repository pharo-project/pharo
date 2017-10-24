tests - some
testSize2
	self assert: empty size equals: 0.
	self assert: full size equals: 4.
	empty add: elementNotIn.
	empty add: $X.
	full add: self elementInForIncludesTest.
	self assert: empty size equals: 2.
	self assert: full size equals: 4.
	empty remove: $X.
	self assert: empty size equals: 1