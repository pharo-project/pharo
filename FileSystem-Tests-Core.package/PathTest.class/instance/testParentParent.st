tests
testParentParent
	| path  |
	path := (Path * '..') parent.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.