private
privateRecomputeIndexesAfterScrollAt: aFloat
	"while rounding fractional part < 0.5"
	|left right|
	
	self invariant.
	
	left := self convertValueToIndex: aFloat.
	right := left + self actualNumberOfVisiblePages - 1.
	firstVisiblePageIndex := left rounded.
	lastVisiblePageIndex := right rounded.
	
	lastVisiblePageIndex > self size ifTrue: [
		|delta|
		delta := lastVisiblePageIndex - self size.
		firstVisiblePageIndex := firstVisiblePageIndex - delta.
		lastVisiblePageIndex := lastVisiblePageIndex - delta.
	].
		
	self invariant.
	
	