operations
expandRectangle: aRectangle
	"Answer a rectangle whose size has been expanded by the receiver which represents each rectangle corner."
	
	|  l r t b |
	l := aRectangle left - self left.
	r :=aRectangle right + self right.
	t := aRectangle top - self top.
	b := aRectangle bottom + self bottom.
	^ Rectangle origin:  l @t extent: ((r - l ) @ (b - t )) 
