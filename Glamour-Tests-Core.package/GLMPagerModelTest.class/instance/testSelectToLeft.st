tests
testSelectToLeft

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	pagerModel switchPaneTo: 6.
	
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	self assert: pagerModel selectedPageIndex equals: 6.
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 5.
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 4.
	self assert: pagerModel lastVisiblePageIndex equals: 5.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 3.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 2.
	self assert: pagerModel firstVisiblePageIndex equals: 2.
	self assert: pagerModel lastVisiblePageIndex equals: 3.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.

	