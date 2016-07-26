rectangle functions
withSideOrCorner: side setToPoint: newPoint
	"Return a copy with side set to newPoint"

	^ self withSideOrCorner: side setToPoint: newPoint minExtent: 0@0