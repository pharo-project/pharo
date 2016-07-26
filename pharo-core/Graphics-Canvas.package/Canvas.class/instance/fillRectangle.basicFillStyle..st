drawing-rectangles
fillRectangle: aRectangle basicFillStyle: aFillStyle
	"Fill the given rectangle with the given, non-composite, fill style
	Note: The default implementation does not recognize any enhanced fill styles."
	
	self fillRectangle: aRectangle color: aFillStyle asColor.