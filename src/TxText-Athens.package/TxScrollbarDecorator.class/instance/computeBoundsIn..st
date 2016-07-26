misc
computeBoundsIn: layoutBounds
	| result |
	result := super computeBoundsIn: layoutBounds.
	
	scrollbar bounds: self bounds.
	^ result
