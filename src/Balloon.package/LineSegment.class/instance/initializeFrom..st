initialize
initializeFrom: controlPoints
	controlPoints size = 2 ifFalse:[self error:'Wrong number of control points'].
	start := controlPoints at: 1.
	end := controlPoints at: 2.