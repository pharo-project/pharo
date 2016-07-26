transforming points
globalPointToLocal: aPoint
	"Transform aPoint from global coordinates into local coordinates"
	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>
	^(self invertPoint: aPoint) rounded