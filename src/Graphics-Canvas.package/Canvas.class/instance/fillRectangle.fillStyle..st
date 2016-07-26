drawing-rectangles
fillRectangle: aRectangle fillStyle: aFillStyle
	"Fill the given rectangle. Double-dispatched via the fill style."
	
	aFillStyle fillRectangle: aRectangle on: self