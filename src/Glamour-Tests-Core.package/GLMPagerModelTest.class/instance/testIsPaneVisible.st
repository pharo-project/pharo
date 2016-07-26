tests
testIsPaneVisible

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	self assert: (pagerModel isPaneVisible: 1) equals: false.
	self assert: (pagerModel isPaneVisible: 2) equals: false.
	self assert: (pagerModel isPaneVisible: 3) equals: false.
	self assert: (pagerModel isPaneVisible: 4) equals: false.
	self assert: (pagerModel isPaneVisible: 5) equals: true.
	self assert: (pagerModel isPaneVisible: 6) equals: true.