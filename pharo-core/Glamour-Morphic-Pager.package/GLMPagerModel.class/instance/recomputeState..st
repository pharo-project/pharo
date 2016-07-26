as yet unclassified
recomputeState: isSmooth
	"recomputes current model state and if necessary notifies all listeners about changes"
	|tmpNumberOfVisiblePages notifyNumberOfVisiblePagesChanged|
	
	notifyNumberOfVisiblePagesChanged := false.
	
	self invariant.
	
	((virtualFirstVisiblePageIndex isNil) & (virtualLastVisiblePageIndex isNil)) ifTrue: [ ^ self ].
	virtualFirstVisiblePageIndex ifNil: [ virtualFirstVisiblePageIndex := self firstVisiblePageIndex ].
	virtualLastVisiblePageIndex ifNil: [ virtualLastVisiblePageIndex := self lastVisiblePageIndex ].
	
	tmpNumberOfVisiblePages := virtualLastVisiblePageIndex - virtualFirstVisiblePageIndex + 1.
	tmpNumberOfVisiblePages = numberOfVisiblePages ifFalse: [ notifyNumberOfVisiblePagesChanged := true ].
	numberOfVisiblePages := tmpNumberOfVisiblePages.
	
	firstVisiblePageIndex := virtualFirstVisiblePageIndex.
	lastVisiblePageIndex := virtualLastVisiblePageIndex.

	self invariant.
	
	virtualFirstVisiblePageIndex = nil.
	virtualLastVisiblePageIndex = nil.
	
	notifyNumberOfVisiblePagesChanged ifTrue: [ self notifyNumberOfVisiblePagesChanged ].
	self notifyScrollPositionChanged: isSmooth.
	