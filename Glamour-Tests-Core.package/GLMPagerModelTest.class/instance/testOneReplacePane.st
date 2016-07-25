tests
testOneReplacePane
	
	pagerModel popAndReplacePane: self createSimplePaneMorph.

	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	