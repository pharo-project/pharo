scrollbars
vShowScrollbar
	"Show the vertical scrollbar."
	
	self vResizeScrollbar.
	self vScrollbarShowing ifTrue: [^self].
	self privateAddMorph: self vScrollbar atIndex: 1.
	self hResizeScrollbar.
	self resizeScroller