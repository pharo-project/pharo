scrollbars
hUpdateScrollbar
	"Update the visibility and dimensions of the horizontal scrollbar as needed."
	
	self hScrollbarNeeded
		ifTrue: [self
					hShowScrollbar;
					hResizeScrollbar]
		ifFalse: [self hHideScrollbar]