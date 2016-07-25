operations
extendRectangle: aRectangle
	"Answer a rectangle whose size has been expanded (without changing its origin) by the receiver which represents each rectangle corner."
	
	^ Rectangle origin: aRectangle origin corner: aRectangle corner + self rightBottom 
