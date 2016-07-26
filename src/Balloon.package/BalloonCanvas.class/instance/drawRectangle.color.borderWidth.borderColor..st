balloon drawing
drawRectangle: r color: c borderWidth: borderWidth borderColor: borderColor
	"Draw a rectangle"

	self ensuredEngine
		drawRectangle: r
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.