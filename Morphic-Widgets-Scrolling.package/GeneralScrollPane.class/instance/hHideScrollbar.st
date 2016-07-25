scrollbars
hHideScrollbar
	"Hide the horizontal scrollbar."
	
	self hScrollbarShowing ifFalse: [^self].
	self removeMorph: self hScrollbar.
	self vResizeScrollbar.
	self resizeScroller