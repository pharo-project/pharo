balloon drawing
drawOval: r color: c borderWidth: borderWidth borderColor: borderColor
	"Draw the oval defined by the given rectangle"

	self ensuredEngine
		drawOval: r
		fill: c
		borderWidth: borderWidth
		borderColor: borderColor
		transform: transform.