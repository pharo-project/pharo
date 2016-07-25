*Morphic-Base
dragThroughOnDesktop: evt
	"Draw out a selection rectangle"
	| selection |
	selection := SelectionMorph newBounds: (evt cursorPoint extent: 8@8).
	self addMorph: selection.
	^ selection extendByHand: evt hand
