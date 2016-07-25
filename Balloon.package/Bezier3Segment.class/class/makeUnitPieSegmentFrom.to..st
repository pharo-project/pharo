utilities
makeUnitPieSegmentFrom: angle1 to: angle2
	"Create a clockwise unit pie segment from angle1 to angle2, that is a pie segment for a circle centered at zero with radius one. Note: This method can be used to create at most a quarter circle."
	| pt1 pt2 rad1 rad2 |
	rad1 := angle1 degreesToRadians.
	rad2 := angle2 degreesToRadians.
	pt1 := rad1 sin @ rad1 cos negated.
	pt2 := rad2 sin @ rad2 cos negated.
	^self makeUnitPieSegmentWith: pt1 and: pt2