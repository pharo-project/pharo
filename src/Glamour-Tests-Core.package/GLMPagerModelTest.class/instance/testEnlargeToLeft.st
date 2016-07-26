tests
testEnlargeToLeft
	|counter right |
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
		
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel numberOfVisiblePages.
	"position of right edge is a constant while enlarging to left"
	right := pagerModel lastVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
	self assert: pagerModel lastVisiblePageIndex equals: right.
	
	"enlarge until numberOfVisible pages > 1"
	[ counter < 6 ] whileTrue: [
		counter := counter + 1.
		pagerModel enlargeToLeft.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
	].

	"now the number of visible pages must be not bigger then 6"
	(1 to: 3) do: [ :i|
		pagerModel enlargeToLeft.
		self assert: pagerModel numberOfVisiblePages equals: 6.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
		self assert: pagerModel firstVisiblePageIndex equals: 1.
	].
