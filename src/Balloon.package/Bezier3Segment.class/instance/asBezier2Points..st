converting
asBezier2Points: error
	"Demote a cubic bezier to a set of approximating quadratic beziers.
	Should convert to forward differencing someday"

	| curves pts step prev index a b f |
	curves := self bezier2SegmentCount: error.
	pts := Array new: curves * 3.
	step := 1.0 / (curves * 2).
	prev := start.
	1 to: curves do: [ :c |
		index := 3*c.
		a := pts at: index-2 put: prev.
		b := (self valueAt: (c*2-1)*step).
		f := pts at: index put: (self valueAt: (c*2)*step).
		pts at: index-1 put: (4 * b - a - f) / 2.
		prev := pts at: index.
		].
	^ pts.
	