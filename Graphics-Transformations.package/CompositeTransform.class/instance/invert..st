transformations
invert: aPoint
	^ globalTransform invert: (localTransform invert: aPoint)