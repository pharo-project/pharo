event handling
reframedTo: aPoint 
	self
		bounds: (self bounds withSideOrCorner: location setToPoint: aPoint)