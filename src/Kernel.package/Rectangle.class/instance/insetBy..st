rectangle functions
insetBy: delta 
	"Answer a Rectangle that is inset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."
	
	^ delta asMargin insetRectangle: self