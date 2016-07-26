brick-geometry
globalOuterBounds
	"converts local outer bounds to global inner bounds on fly"
	
	^ self owner ifNotNil: [ self outerBounds translateBy: self owner globalBoundsTranslateFactor ] ifNil: [ self outerBounds ].