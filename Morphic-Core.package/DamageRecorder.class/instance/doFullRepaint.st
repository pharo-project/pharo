recording
doFullRepaint
	"Record that a full redisplay is needed. No further damage rectangles will be recorded until after the next reset."

	^ totalRepaint := true.
