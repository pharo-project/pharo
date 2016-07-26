example
example1
	| stringMorph transformationMorph |
	stringMorph := 'vertical text' asMorph.
	transformationMorph := TransformationMorph new asFlexOf: stringMorph.
	transformationMorph angle: Float pi / 2.
	transformationMorph position: 5@5.
	transformationMorph openInWorld.