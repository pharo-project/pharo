tests
testDiminishToRight
	|counter right|
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel defaultNumberOfVisiblePages.
	"position of right edge is a constant while diminishing to right"
	right := pagerModel lastVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
	self assert: pagerModel lastVisiblePageIndex equals: right.
	
	"diminish until numberOfVisible pages > 1"
	[ counter > 1 ] whileTrue: [
		pagerModel diminishToRight.
		counter := counter - 1.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
	].

	"now the number of visible pages must be not less then 1"
	(1 to: 3) do: [ :i|
		pagerModel diminishToRight.
		self assert: pagerModel numberOfVisiblePages equals: 1.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
	].
