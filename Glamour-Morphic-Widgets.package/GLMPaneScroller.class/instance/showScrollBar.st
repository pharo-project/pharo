scrolling
showScrollBar
	self scrollBarIsVisible ifTrue: [^ self].
	self resizeScrollBar.
	self addMorphFront: scrollBar.
	self adjustPaneHeight.
	