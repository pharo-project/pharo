as yet unclassified
desiredMoreMorphPosition
	"Answer the position the more morph should be placed."

	^self moreButton boundsInWorld bottom + self moreMorph height > self world height
		ifTrue: [self moreButton boundsInWorld topRight - (self moreMorph width@self moreMorph height)]
		ifFalse: [self moreButton boundsInWorld bottomRight - (self moreMorph width@0)]