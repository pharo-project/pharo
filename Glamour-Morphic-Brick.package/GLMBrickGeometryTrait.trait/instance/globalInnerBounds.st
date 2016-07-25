brick-geometry
globalInnerBounds
	"converts local inner bounds to global inner bounds on fly"
	
	^ self owner ifNotNil: [ self innerBounds translateBy: self owner globalBoundsTranslateFactor ] ifNil: [ self innerBounds ].