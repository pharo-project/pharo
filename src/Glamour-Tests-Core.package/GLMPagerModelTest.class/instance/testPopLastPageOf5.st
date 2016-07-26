tests
testPopLastPageOf5
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel popPane.
	
	self assert: pagerModel size equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	self assert: pagerModel numberOfVisiblePages equals: 2.