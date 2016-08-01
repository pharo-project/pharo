operations
expandRectangle: aRectangle
	"Answer a rectangle whose size has been expanded by the receiver which represents each rectangle corner."
	
	^ Rectangle 
		left: aRectangle left - self left 
		right: aRectangle right + self right 
		top: aRectangle top - self top 
		bottom: aRectangle bottom + self bottom 
