scrollbars
hShowScrollbar
	"Show the horizontal scrollbar."
	
	self hResizeScrollbar.
	self hScrollbarShowing ifTrue: [^self].
	self privateAddMorph: self hScrollbar atIndex: 1.
	self vResizeScrollbar.
	self resizeScroller