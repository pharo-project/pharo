brick-geometry
globalBounds
	"converts local bounds to global bounds on fly"
	
	^ self owner ifNotNil: [ self brickBounds translateBy: self owner globalBoundsTranslateFactor ] ifNil: [ self brickBounds ].