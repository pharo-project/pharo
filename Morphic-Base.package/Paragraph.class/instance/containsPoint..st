selection
containsPoint: aPoint
	^ (lines at: (self lineIndexForPoint: aPoint)) rectangle
		containsPoint: aPoint