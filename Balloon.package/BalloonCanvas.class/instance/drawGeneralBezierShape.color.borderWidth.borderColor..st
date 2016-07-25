balloon drawing
drawGeneralBezierShape: contours color: c borderWidth: borderWidth borderColor: borderColor
	"Draw a general boundary shape (e.g., possibly containing holes)"

	self ensuredEngine
		drawGeneralBezierShape: contours
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.