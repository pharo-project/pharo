initialize
initializeFrom: controlPoints
	controlPoints size = 3 ifFalse:[self error:'Wrong number of control points'].
	start := controlPoints at: 1.
	via := controlPoints at: 2.
	end := controlPoints at: 3.