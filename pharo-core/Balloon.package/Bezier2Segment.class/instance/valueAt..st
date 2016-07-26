vector functions
valueAt: parameter
	"Evaluate the receiver at the given parametric value"
	"Return the point at the parametric value t:
		p(t) =	(1-t)^2 * p1 + 
				2*t*(1-t) * p2 + 
				t^2 * p3.
	"
	| t1 t2 t3 |
	t1 := (1.0 - parameter) squared.
	t2 := 2 * parameter * (1.0 - parameter).
	t3 := parameter squared.
	^(start * t1) + (via * t2) + (end * t3)