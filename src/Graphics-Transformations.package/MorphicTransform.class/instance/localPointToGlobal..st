transforming points
localPointToGlobal: aPoint
	"Transform aPoint from global coordinates into local coordinates"
	^self invert: aPoint