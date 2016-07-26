layout
layoutBounds: aRectangle
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	| outer inner |
	outer := self bounds.
	inner := self layoutBounds.
	bounds := aRectangle origin + (outer origin - inner origin) corner:
				aRectangle corner + (outer corner - inner corner).