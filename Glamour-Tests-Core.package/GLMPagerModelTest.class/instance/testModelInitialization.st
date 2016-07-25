tests
testModelInitialization

	self assert: pagerModel size equals: 0.
	self assert: pagerModel firstVisiblePageIndex equals: 0.
	self assert: pagerModel lastVisiblePageIndex equals: 0.
	self assert: pagerModel actualNumberOfVisiblePages equals: 0.
	self assert: pagerModel numberOfVisiblePages equals: 2.