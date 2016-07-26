drawing-rectangles
frameRectangle: aRectangle width: width colors: colors dashes: dashes
	"Draw a rectangle with the given width, colors and dash lengths."

	self
		frameRectangle: aRectangle
		width: width
		colors: colors
		dashes: dashes
		offset: self origin