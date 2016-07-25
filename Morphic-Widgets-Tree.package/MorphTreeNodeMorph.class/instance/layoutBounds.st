layout
layoutBounds
	"Return the bounds for laying out children of the receiver"

	| lb left right |
	
	lb := super layoutBounds.
	container ifNil: [ ^ lb ].
	left := (lb left + self spacerWidth).
	right := (lb right max: left + self spacerWidth).
	"Make sure that left and right have changed simultaneously, avoiding creating degenerate rectangle"
	^ Rectangle left: left right: right top: lb top bottom: lb bottom
