converting
asBezier3Segment
	"Represent the receiver as cubic bezier segment"
	^Bezier3Segment
		from: start
		via: 2*via+start / 3.0
		and: 2*via+end / 3.0
		to: end