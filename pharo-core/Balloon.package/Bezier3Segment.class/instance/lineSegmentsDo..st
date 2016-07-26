vector functions
lineSegmentsDo: aBlock
	"Evaluate aBlock with the receiver's line segments"
	"Note: We could use forward differencing here."
	| steps last deltaStep t next |
	steps := 1 max: (self length // 10). "Assume 10 pixels per step"
	last := start.
	deltaStep := 1.0 / steps asFloat.
	t := deltaStep.
	1 to: steps do:[:i|
		next := self valueAt: t.
		aBlock value: last value: next.
		last := next.
		t := t + deltaStep].