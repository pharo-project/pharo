drawing
fillColor: c
	"Note: This always fills, even if the color is transparent."
	"Note2: To achieve the above we must make sure that c is NOT transparent"
	self frameAndFillRectangle: form boundingBox 
		fillColor: (c alpha: 1.0)
		borderWidth: 0
		borderColor: nil