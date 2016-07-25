layout
doLayoutIn: layoutBounds 
	"Compute a new layout based on the given layout bounds."

	|scrollbarChange|
	super doLayoutIn: layoutBounds.
	scrollbarChange := (self vScrollbarShowing = self vScrollbarNeeded) not.
	scrollbarChange := scrollbarChange or: [(self hScrollbarShowing = self hScrollbarNeeded) not].
	self	updateScrollbars.
	scrollbarChange ifFalse: [self resizeScroller]. "if there is a scrollbar change then done already"
	super doLayoutIn: layoutBounds
