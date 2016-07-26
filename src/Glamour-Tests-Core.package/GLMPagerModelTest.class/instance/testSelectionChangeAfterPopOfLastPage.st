tests
testSelectionChangeAfterPopOfLastPage
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	
	pagerModel selectedPageIndex: 5.
	self assert: pagerModel selectedPageIndex equals: 5.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 4.
	self assert: pagerModel size equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 3.
	self assert: pagerModel size equals: 3.
	self assert: pagerModel firstVisiblePageIndex equals: 2.
	self assert: pagerModel lastVisiblePageIndex equals: 3.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 2.
	self assert: pagerModel size equals: 2.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.