geometry
textPositionAt: aPoint
	"Answer the text position closest to given point in view"
	^ self layout textPositionAt: aPoint + offset 