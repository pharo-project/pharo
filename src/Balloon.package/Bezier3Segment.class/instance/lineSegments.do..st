vector functions
lineSegments: steps do: aBlock
	"Evaluate aBlock with the receiver's line segments"
	"Note: We could use forward differencing here."
	| last deltaStep t next |
	last := start.
	deltaStep := 1.0 / steps asFloat.
	t := deltaStep.
	1 to: steps do:[:i|
		next := self valueAt: t.
		aBlock value: last value: next.
		last := next.
		t := t + deltaStep].