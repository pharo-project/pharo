smoothing
slopes: knots 
	"Choose slopes according to state of polygon"
	self isCurvy ifFalse: [^ self segmentedSlopesOf: knots ].
	^ self closedCubicSlopesOf: knots