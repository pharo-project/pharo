scrolling
hHideOrShowScrollBar
	"Hide or show the scrollbar depending on if the pane is scrolled/scrollable."

	self hIsScrollbarNeeded
		ifTrue:[ self hShowScrollBar ]
		ifFalse: [ self hHideScrollBar ].
