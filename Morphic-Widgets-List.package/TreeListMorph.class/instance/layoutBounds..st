private
layoutBounds: aRectangle
	"Set the bounds for laying out children of the receiver.
	Update the scroller and scrollbars now since bounds set directly
	(not via #extent:)."
	
	super layoutBounds: aRectangle.
	self
		resizeScroller;
		setScrollDeltas