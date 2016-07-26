drawing-rectangles
frameRectangle: r width: w color: c
	"Draw a frame around the given rectangle"
	^self frameAndFillRectangle: r
			fillColor: Color transparent
			borderWidth: w
			borderColor: c