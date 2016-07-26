rectangle functions
expandBy: delta 
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."
	
	^ delta asMargin expandRectangle: self