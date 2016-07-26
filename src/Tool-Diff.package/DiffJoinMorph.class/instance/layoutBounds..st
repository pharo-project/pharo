layout
layoutBounds: aRectangle
	"Set the bounds for laying out children of the receiver."
	
	super layoutBounds: aRectangle.
	self updateMappings