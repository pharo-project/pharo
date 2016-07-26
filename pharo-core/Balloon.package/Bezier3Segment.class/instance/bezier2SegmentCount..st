converting
bezier2SegmentCount: pixelError
	"Compute the number of quadratic bezier segments needed to approximate
	this cubic with no more than a specified error"
	| a |
	a := (start x negated @ start y negated) + (3 * via1) - (3 * via2) +
(end).
	^ (((a r / (20.0 * pixelError)) raisedTo: 0.333333) ceiling) max: 1.

