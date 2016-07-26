initialize
from: startPoint to: endPoint
	"Initialize the receiver as straight line"
	start := startPoint.
	end := endPoint.
	via := (start + end) // 2.