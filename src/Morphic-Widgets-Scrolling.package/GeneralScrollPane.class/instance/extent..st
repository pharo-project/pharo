geometry
extent: newExtent
	"Update the receiver's extent. Hide/show the scrollbars and resize the scroller
	as neccessary."
	
	|scrollbarChange|
	bounds extent = newExtent ifTrue: [^ self].
	super extent: newExtent.
	scrollbarChange := (self vScrollbarShowing = self vScrollbarNeeded) not.
	scrollbarChange := scrollbarChange or: [(self hScrollbarShowing = self hScrollbarNeeded) not].
	self	updateScrollbars.
	scrollbarChange ifFalse: [self resizeScroller] "if there is a scrollbar change then done already"