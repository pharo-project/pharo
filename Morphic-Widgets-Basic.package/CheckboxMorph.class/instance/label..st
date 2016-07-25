accessing
label: aStringOrMorph
	"Set the label morph or label morph contents."

	self labelMorph: (aStringOrMorph isMorph
		ifTrue: [aStringOrMorph]
		ifFalse: [self labelMorph contents: aStringOrMorph.
				self newLabel])