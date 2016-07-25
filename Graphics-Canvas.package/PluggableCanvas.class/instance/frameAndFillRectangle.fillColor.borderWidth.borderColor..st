drawing-rectangles
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	self apply: [ :c |
		c frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor ]