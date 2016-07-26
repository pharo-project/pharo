drawing
fillRectangle: r color: c
	"Fill the rectangle with the given color"
	^self frameAndFillRectangle: r
			fillColor: c
			borderWidth: 0
			borderColor: nil