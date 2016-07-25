scrolling
switchPaneTo: anIndex
	| normalizedIndex |
	"switches pane to specified. Works as scrollToPane, but additionaly selects and focuses that pane."
	
	self invariant.
	
	normalizedIndex := self normalizePaneIndex: anIndex.
	
	self scrollToPane: normalizedIndex.	
	self focusPaneAt: normalizedIndex.
	
	selectedPageIndex := normalizedIndex.
	self notifySelectedPageChanged.
	
	self invariant.