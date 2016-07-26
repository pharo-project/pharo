drawing-rectangles
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	"Draw the rectangle using the given attributes"
	myCanvas
		frameAndFillRectangle: r
		fillColor: (self mapColor: fillColor)
		borderWidth: borderWidth
		borderColor: (self mapColor: borderColor)