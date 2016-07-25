scrollbars
vUpdateScrollbar
	"Update the visibility and dimensions of the vertical scrollbar as needed."
	
	self vScrollbarNeeded
		ifTrue: [self
				vShowScrollbar;
				vResizeScrollbar]
		ifFalse: [self vHideScrollbar]