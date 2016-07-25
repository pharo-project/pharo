tests
testPushPanesAfterOpen

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	self openInWindow.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel size equals: 3.
	self assert: self panesMorphs size equals: 3.
	self assert: self buttonsMorphs size equals: 3.
	
	
