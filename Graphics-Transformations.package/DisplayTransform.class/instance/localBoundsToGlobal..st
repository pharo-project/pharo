transforming rects
localBoundsToGlobal: aRectangle
	"Transform aRectangle from local coordinates into global coordinates"
	^Rectangle encompassing: (self localPointsToGlobal: aRectangle corners)