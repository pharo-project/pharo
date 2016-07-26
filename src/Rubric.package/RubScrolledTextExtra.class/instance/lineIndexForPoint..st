accessing
lineIndexForPoint: aPoint
	^ self paragraph lineIndexForPoint: aPoint  + (0 @ (self offset y - self bounds top))