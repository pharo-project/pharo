scrolling
hShowScrollBar

	self hIsScrollbarShowing ifTrue: [^self].
	self hResizeScrollBar.
	self privateAddMorph: hScrollBar atIndex: 1.
	self resetExtent.
