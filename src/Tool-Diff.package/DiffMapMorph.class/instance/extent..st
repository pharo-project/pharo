accessing
extent: newExtent
	"Update the gradient."
	
	super extent: newExtent.
	(self fillStyle notNil and: [self fillStyle isOrientedFill])
		ifTrue: [self fillStyle direction: 0@self height]