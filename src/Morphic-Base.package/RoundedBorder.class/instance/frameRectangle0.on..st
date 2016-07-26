drawing
frameRectangle0: aRectangle on: aCanvas
	"Draw the border for a corner radius of 0."

	aCanvas
		frameAndFillRectangle: aRectangle
		fillColor: Color transparent
		borderWidth: self width
		borderColor: self color