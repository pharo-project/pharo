scrolling
vHideScrollBar
	self vIsScrollbarShowing ifFalse: [^self].
	self removeMorph: scrollBar.
	self resetExtent
	
