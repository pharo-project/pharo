transforming rects
globalBoundsToLocal: aRectangle
	"Transform aRectangle from global coordinates into local coordinates"
	^Rectangle encompassing: (self globalPointsToLocal: aRectangle corners)