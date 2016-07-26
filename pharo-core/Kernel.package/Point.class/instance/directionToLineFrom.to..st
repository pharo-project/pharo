point functions
directionToLineFrom: p1 to: p2
	"Answer the direction of the line from the receiver position.
	< 0 => left (receiver to right)
	= => on line
	> 0 => right (receiver to left)."

	^((p2 x - p1 x) * (self y - p1 y)) - ((self x - p1 x) * (p2 y - p1 y))