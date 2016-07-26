geometry
extent: newExtent
	"Update the gradient."
	
	super extent: newExtent.
	(self fillStyle notNil and: [self fillStyle isSolidFill not])
		ifTrue: [self fillStyle: self fillStyleToUse]