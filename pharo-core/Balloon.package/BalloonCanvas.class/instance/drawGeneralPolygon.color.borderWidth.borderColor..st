balloon drawing
drawGeneralPolygon: contours color: c borderWidth: borderWidth borderColor: borderColor
	"Draw a general polygon (e.g., a polygon that can contain holes)"

	self ensuredEngine
		drawGeneralPolygon: contours
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.