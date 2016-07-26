transforming rects
localBounds: srcRect toGlobal: dstRect
	"Transform aRectangle from local coordinates into global coordinates"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>
	^super localBoundsToGlobal: srcRect