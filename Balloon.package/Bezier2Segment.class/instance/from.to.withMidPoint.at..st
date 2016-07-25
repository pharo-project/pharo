initialize
from: startPoint to: endPoint withMidPoint: pointOnCurve at: parameter
	"Initialize the receiver with the pointOnCurve at the given parametric value"
	| t1 t2 t3 |
	start := startPoint.
	end := endPoint.
	"Compute via"
	t1 := (1.0 - parameter) squared.
	t2 := 1.0 / (2 * parameter * (1.0 - parameter)).
	t3 := parameter squared.
	via := (pointOnCurve - (start * t1)  - (end * t3)) * t2