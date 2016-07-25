scrolling
hHideScrollBar
	self hIsScrollbarShowing ifFalse: [^scroller offset: (self hMargin negated@scroller offset y)].
	self removeMorph: hScrollBar.
	scroller offset: (self hMargin negated@scroller offset y).
	self resetExtent.

