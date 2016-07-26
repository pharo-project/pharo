slider-testing
isLeftResizingZone: evt
	
	^ (self globalBounds withWidth: resizingZoneWidth) containsPoint: evt position
	
	