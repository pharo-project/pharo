initialization
initializeFrom: controlPoints
	controlPoints size = 4 ifFalse:[self error:'Wrong number of control points'].
	start := controlPoints at: 1.
	via1 := controlPoints at: 2.
	via2 := controlPoints at: 3.
	end := controlPoints at: 4.