path commands
ccwArcTo: endPt angle: rot
	" angle should be specified in radians "
	^ self addSegment: (AthensCCWArcSegment new endPoint: (self toAbsolute: endPt) angle: rot).

