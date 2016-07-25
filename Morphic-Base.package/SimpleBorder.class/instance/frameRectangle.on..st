drawing
frameRectangle: aRectangle on: aCanvas
	aCanvas frameAndFillRectangle: aRectangle
		fillColor: Color transparent
		borderWidth: self width
		topLeftColor: self topLeftColor
		bottomRightColor: self bottomRightColor.