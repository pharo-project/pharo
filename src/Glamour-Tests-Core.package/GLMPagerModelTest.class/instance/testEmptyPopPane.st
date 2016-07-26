tests
testEmptyPopPane
	| count |
	
	count := 0.
	pagerModel announcer when: GLMPagePoped do: [ :ann | count := count + 1 ].
	
	pagerModel popPane.
	
	self assert: count equals: 0.
	
	self assert: pagerModel size equals: 0.
	self assert: pagerModel firstVisiblePageIndex equals: 0.
	self assert: pagerModel lastVisiblePageIndex equals: 0.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	pagerModel popPane.
	
	self assert: count equals: 0.
	