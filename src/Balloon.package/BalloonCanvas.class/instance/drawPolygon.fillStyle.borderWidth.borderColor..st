drawing-polygons
drawPolygon: vertices fillStyle: aFillStyle borderWidth: borderWidth borderColor: borderColor
	"Draw a simple polygon defined by the list of vertices."

	self ensuredEngine
		drawPolygon: (vertices copyWith: vertices first)
		fill: aFillStyle
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.