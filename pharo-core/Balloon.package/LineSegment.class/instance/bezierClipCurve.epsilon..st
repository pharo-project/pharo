bezier clipping
bezierClipCurve: aCurve epsilon: eps
	"Compute the intersection of the receiver (a line) with the given curve using bezier clipping."
	| tMin tMax clip newCurve |
	clip := self bezierClipInterval: aCurve.
	clip ifNil:[^#()]. "no overlap"
	tMin := clip at: 1.
	tMax := clip at: 2.
	newCurve := aCurve curveFrom: tMin to: tMax.
	newCurve length < eps ifTrue:[^Array with: (aCurve valueAt: tMin + tMax * 0.5)].
	(tMin < 0.001 and:[tMax > 0.999]) ifTrue:[
		"Need to split aCurve before proceeding"
		| curve1 curve2 |
		curve1 := aCurve curveFrom: 0.0 to: 0.5.
		curve2 := aCurve curveFrom: 0.5 to: 1.0.
		^(curve1 bezierClipCurve: self epsilon: eps),
			(curve2 bezierClipCurve: self epsilon: eps).
	].
	^newCurve bezierClipCurve: self epsilon: eps.