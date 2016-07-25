initialization
initializeScroller
	scroller := self newTransformMorph color: Color transparent.
	scroller offset: -3 @ 0.
	self addMorph: scroller