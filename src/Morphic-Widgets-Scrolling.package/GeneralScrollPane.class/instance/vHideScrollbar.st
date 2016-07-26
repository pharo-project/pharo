scrollbars
vHideScrollbar
	"Hide the vertical scrollbar."
	
	self vScrollbarShowing ifFalse: [^self].
	self removeMorph: self vScrollbar.
	self hResizeScrollbar.
	self resizeScroller