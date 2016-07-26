focus
focusPaneAt: pageIndex
	"tries to focus previosely focused morph at specified pane by its index"
	
	( self isFocusedMorphVisibleAt: pageIndex ) ifTrue: [
		focusedPageIndex := pageIndex.
		(self focusedMorphAt: pageIndex) activeHand newKeyboardFocus: (self focusedMorphAt: pageIndex).
		self notifyFocusedPageChanged.
	].