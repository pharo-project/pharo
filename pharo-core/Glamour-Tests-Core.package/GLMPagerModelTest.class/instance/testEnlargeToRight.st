tests
testEnlargeToRight
	|counter left|
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	pagerModel scrollTo: 0.0 smooth: false.
	
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel numberOfVisiblePages.
	"position of left edge is a constant while enlargingto right"
	left := pagerModel firstVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
	self assert: pagerModel firstVisiblePageIndex equals: left.
	
	"diminish until numberOfVisible pages > 1"
	[ counter < 6 ] whileTrue: [
		counter := counter + 1.
		pagerModel enlargeToRight.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
	].

	"now the number of visible pages must be not bigger than 6"
	(1 to: 3) do: [ :i|
		pagerModel enlargeToRight.
		self assert: pagerModel numberOfVisiblePages equals: 6.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
		self assert: pagerModel lastVisiblePageIndex equals: 6.
	].
