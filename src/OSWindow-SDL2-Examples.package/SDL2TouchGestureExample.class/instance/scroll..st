manage scrolling
scroll: anEvent
	circleCenter := circleCenter
		+ (anEvent direction * (anEvent dDist abs * (self defaultExtent distanceTo: 0 @ 0)))