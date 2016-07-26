transforming rects
globalBoundsToLocal: aRectangle
	"Transform aRectangle from global coordinates into local coordinates"
	^self globalBounds: aRectangle toLocal: Rectangle new