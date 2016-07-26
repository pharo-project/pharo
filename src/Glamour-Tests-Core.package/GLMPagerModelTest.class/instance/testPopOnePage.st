tests
testPopOnePage
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel popPane.
	
	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.