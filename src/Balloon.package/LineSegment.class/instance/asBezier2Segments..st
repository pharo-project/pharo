converting
asBezier2Segments: error
	"Demote a cubic bezier to a set of approximating quadratic beziers."
	| pts |
	pts := self asBezier2Points: error.
	^(1 to: pts size by: 3) collect:[:i| 
		Bezier2Segment from: (pts at: i) via: (pts at: i+1) to: (pts at: i+2)].
