transforming rects
localBoundsToGlobal: aRectangle
	"Transform aRectangle from local coordinates into global coordinates"
	^self localBounds: aRectangle toGlobal: Rectangle new