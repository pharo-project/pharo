transforming points
localPointToGlobal: aPoint
	"Transform aPoint from global coordinates into local coordinates"
	^globalTransform localPointToGlobal:
		(localTransform localPointToGlobal: aPoint)