cubic support
segmentedSlopesOf: knots 
	"For a collection of floats. Returns the slopes for straight 
	segments between vertices."
	"last slope closes the polygon. Always return same size as 
	self. "
	^ knots collectWithIndex: [:x :i | (knots atWrap: i + 1) - x]