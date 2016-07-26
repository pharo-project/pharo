private-geometry
globalBoundsTranslateFactor

	^ self owner ifNotNil: [ self owner globalBoundsTranslateFactor + self brickBounds origin ] ifNil: [ 0@0 ]