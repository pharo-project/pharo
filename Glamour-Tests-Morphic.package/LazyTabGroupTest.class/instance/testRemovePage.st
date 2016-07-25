tests
testRemovePage
	tabs removePage: tabs pages first.
	self assert: tabs pages size equals: 1