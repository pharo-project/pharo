access
scroller: aTransformMorph
	scroller ifNotNil:[scroller delete].
	scroller := aTransformMorph.
	self addMorph: scroller.
	self resizeScroller.