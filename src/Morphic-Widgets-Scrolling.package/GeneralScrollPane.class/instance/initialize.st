initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		scroller: self newScroller;
		hScrollbar: self newHScrollbar;
		vScrollbar: self newVScrollbar;
		scrollTarget: self defaultScrollTarget.
	self
		addMorph: self scroller;
		resizeScroller