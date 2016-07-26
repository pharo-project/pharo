primitives
deltaFrom: x1 to: x2 nSteps: n 
	"Utility routine for computing Warp increments.
	x1 is starting pixel, x2 is ending pixel;  assumes n >= 1"
	| fixedPtOne |
	fixedPtOne := 16384.	"1.0 in fixed-pt representation"
	x2 > x1 
		ifTrue: [ ^ (x2 - x1 + fixedPtOne) // (n + 1) + 1 ]
		ifFalse: 
			[ x2 = x1 ifTrue: [ ^ 0 ].
			^ 0 - ((x1 - x2 + fixedPtOne) // (n + 1) + 1) ]