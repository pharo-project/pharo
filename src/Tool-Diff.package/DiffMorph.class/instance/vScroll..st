updating
vScroll: scrollValue
	"Called from standalone scroolbar.
	Scroll the srcMorph and redo the join."

	self srcMorph scrollPane vScrollbarValue: scrollValue.
	self dstMorph scrollPane vScrollbarValue: scrollValue.
	self updateJoinOffsets