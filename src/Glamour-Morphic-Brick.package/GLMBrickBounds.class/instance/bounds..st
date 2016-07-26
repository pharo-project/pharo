accessing - bounds
bounds: aRectangle
	"changes bounds and marks width or height as dirty if it changes"
	self updateState: aRectangle.
	self brickBounds: aRectangle