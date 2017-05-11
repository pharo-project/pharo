brick-geometry
globalClippingBounds
	^ self owner
		ifNotNil: [ self drawingBounds translateBy: self owner globalBoundsTranslateFactor ]
		ifNil: [ self drawingBounds ]