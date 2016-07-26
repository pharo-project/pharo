tests
testPrivateRecomputeIndexesAfterScrollAt

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	
	"raw first visible must be 1.0, last visible 1.0 and after rounding 1 and 1"
	pagerModel privateRecomputeIndexesAfterScrollAt: 1.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	
	"raw first visible must be 1.0, last visible 1.0 and after rounding 1 and 1"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
		
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
		
	"raw first visible must be 1.0, last visible 1.0 and after rounding 1 and 1"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.
	
	"raw first visible must be 4.4, last visible 5.4 and after rounding 4 and 5"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.85.
	
	self assert: pagerModel firstVisiblePageIndex equals: 4.
	self assert: pagerModel lastVisiblePageIndex equals: 5.
	
	"raw first visible must be 4.6, last visible 5.6 and after rounding 5 and 6"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.9.
	
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	"raw first visible must be 4.5, last visible 5.5 and after rounding 5 and 6"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.875.
	
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	"raw first visible must be 5.0, last visible 6.0 and after rounding 5 and 6"
	pagerModel privateRecomputeIndexesAfterScrollAt: 1.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.