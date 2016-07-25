transforming points
globalPointToLocal: aPoint
	"Transform aPoint from global coordinates into local coordinates"
	^localTransform globalPointToLocal:
		(globalTransform globalPointToLocal: aPoint)