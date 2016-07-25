scrolling
vShowScrollBar

	self vIsScrollbarShowing ifTrue: [^ self].
	self vResizeScrollBar.
	self privateAddMorph: scrollBar atIndex: 1.
	self resetExtent
