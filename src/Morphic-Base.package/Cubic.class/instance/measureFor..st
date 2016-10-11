cubic support
measureFor: n 
	"Return a distance measure for cubic curve with n segments. 
	For convienence and accuracy we use the sum of the distances. "
	"The first point is poly of 0."
	| p1 measure |
	p1 := self first.
	measure := 0.
	1 to: n do: [:i | | p2 | 
			p2 := self polynomialEval: i / n asFloat.
			measure := measure + (p2 distanceTo: p1).
			p1 := p2].
	^ measure