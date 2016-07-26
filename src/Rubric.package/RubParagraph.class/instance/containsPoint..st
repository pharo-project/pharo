querying
containsPoint: aPoint
	^ (self lines at: (self lineIndexForPoint: aPoint)) rectangle
		containsPoint: aPoint