initialize
from: startPoint to: endPoint withMidPoint: pointOnCurve
	"Initialize the receiver with the pointOnCurve assumed at the parametric value 0.5"
	start := startPoint.
	end := endPoint.
	"Compute via"
	via := (pointOnCurve * 2) - (start + end * 0.5).