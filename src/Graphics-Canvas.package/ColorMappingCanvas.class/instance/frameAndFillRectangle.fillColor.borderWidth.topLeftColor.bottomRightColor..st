drawing-rectangles
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"Draw the rectangle using the given attributes"
	myCanvas
		frameAndFillRectangle: r 
		fillColor: (self mapColor: fillColor) 
		borderWidth: borderWidth 
		topLeftColor: (self mapColor: topLeftColor)
		bottomRightColor: (self mapColor: bottomRightColor)