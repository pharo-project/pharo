accessing
selectedPageIndex: anIndex
	| normalizedIndex |
	"changes current selected page and scrolls to it if necessary. Also if possible re-focused available morph"
	
	normalizedIndex := self normalizePaneIndex: anIndex.
	
	(self isPaneVisible: normalizedIndex) ifFalse: [
		"we need to scroll if new selected page is invisible"
		self scrollToPane: (normalizedIndex < self firstVisiblePageIndex ifTrue: [ normalizedIndex ] ifFalse: [(normalizedIndex - self numberOfVisiblePages + 1)]).
	].
	
	selectedPageIndex := normalizedIndex.
	self focusPaneAt: normalizedIndex.
	self notifySelectedPageChanged.

	self invariant.