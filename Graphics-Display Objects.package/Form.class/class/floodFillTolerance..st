settings
floodFillTolerance: aFloat
	(aFloat >= 0.0 and: [aFloat < 0.3])
		ifTrue: [FloodFillTolerance := aFloat]
		ifFalse: [FloodFillTolerance := 0.0]