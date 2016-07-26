scrolling
scrollTo: aFloat smooth: isSmooth callback: aBlock
	"notifies all listeners to scroll at the specified value"
	
	self privateRecomputeIndexesAfterScrollAt: aFloat.
	self notifyScrollPositionChanged: isSmooth callback: aBlock