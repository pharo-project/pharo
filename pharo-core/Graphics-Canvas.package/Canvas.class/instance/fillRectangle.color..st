drawing-rectangles
fillRectangle: r color: c
	"Fill the rectangle using the given color"
	^self 
		frameAndFillRectangle: r
		fillColor: c
		borderWidth: 0
		borderColor: Color transparent