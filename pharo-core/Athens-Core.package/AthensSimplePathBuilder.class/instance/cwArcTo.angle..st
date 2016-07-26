path commands
cwArcTo: endPt angle: rot
	" angle should be specified in radians "
	^ self addSegment: (AthensCWArcSegment new endPoint: (self toAbsolute: endPt) angle: rot).

