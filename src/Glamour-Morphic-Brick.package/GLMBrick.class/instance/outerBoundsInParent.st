brick-geometry
outerBoundsInParent
	^ self owner
		ifNotNil: [ self wrappedBounds outerBounds translateBy: self owner brickBounds origin ]
		ifNil: [ self wrappedBounds outerBounds ]