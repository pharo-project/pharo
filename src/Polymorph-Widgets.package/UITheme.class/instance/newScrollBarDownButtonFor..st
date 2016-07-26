morph creation
newScrollBarDownButtonFor: aScrollbar
	^ BorderedMorph newBounds: (self scrollBarDownButtonBoundsFor: aScrollbar)