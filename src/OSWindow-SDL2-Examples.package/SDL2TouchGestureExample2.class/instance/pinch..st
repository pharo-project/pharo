manage pinch
pinch: anEvent
	circleSize := circleSize + (anEvent dDist * (self defaultExtent distanceTo: 0@0))