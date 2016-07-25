event handling
transformFromOutermostWorld
	"Return a transform to map world coordinates into my local coordinates"

	"self isWorldMorph ifTrue: [^ MorphicTransform identity]."
	^ self transformFrom: self outermostWorldMorph