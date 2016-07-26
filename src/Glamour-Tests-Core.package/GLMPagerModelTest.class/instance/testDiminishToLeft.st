tests
testDiminishToLeft
	|counter left|
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel numberOfVisiblePages.
	"position of left edge is a constant while diminishing to left"
	left := pagerModel firstVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
	self assert: pagerModel firstVisiblePageIndex equals: left.
	
	"diminish until numberOfVisible pages > 1"
	[ counter > 1 ] whileTrue: [
		pagerModel diminishToLeft.
		counter := counter - 1.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
	].

	"now the number of visible pages must be not less then 1"
	(1 to: 3) do: [ :i|
		pagerModel diminishToLeft.
		self assert: pagerModel numberOfVisiblePages equals: 1.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
	].
