layout
layoutBounds: aRectangle
	"Set the bounds for laying out children of the receiver.
	Update the fillstyle since it may depend on the bounds."
	
	super layoutBounds: aRectangle.
	self gradientLook ifTrue: [
		self fillStyle isOrientedFill
			ifTrue: [self fillStyle: self fillStyleToUse]]