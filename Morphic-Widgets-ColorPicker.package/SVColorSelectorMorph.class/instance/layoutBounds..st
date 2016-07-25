layout
layoutBounds: aRectangle
	"Set the bounds for laying out children of the receiver.
	Note: written so that #layoutBounds can be changed without touching this method"
	
	super layoutBounds: aRectangle.
	self updateGradients