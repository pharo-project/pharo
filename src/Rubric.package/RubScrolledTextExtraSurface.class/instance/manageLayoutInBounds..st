geometry
manageLayoutInBounds: aRectangle
	self position: (self computedPositionFrom: aRectangle).
	self extent: (self computedExtentFrom: aRectangle).
	self changed.
	^ aRectangle 