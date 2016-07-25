accessing
frameRectangle: aRectangle on: aCanvas
	"Reduce width by the margin."
	
	aCanvas frameAndFillRectangle: aRectangle
		fillColor: Color transparent
		borderWidth: (self width - self margin max: 0)
		topLeftColor: self topLeftColor
		bottomRightColor: self bottomRightColor.