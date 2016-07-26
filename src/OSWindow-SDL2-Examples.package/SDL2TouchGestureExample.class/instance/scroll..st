manage scrolling
scroll: anEvent
	circleCenter := circleCenter
		+ (anEvent direction * (anEvent dDist abs * (self defaultExtent dist: 0 @ 0)))