slider-testing
isRightResizingZone: evt
	|resizingRectangle top left globalBounds|
	globalBounds := self globalBounds.
	
	top := globalBounds top.
	left := globalBounds right - resizingZoneWidth.
	
	resizingRectangle := left@top corner: globalBounds corner.
	
	^ resizingRectangle containsPoint: evt position.
	
	