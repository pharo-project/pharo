what to view
morphToView
	"If the receiver is viewing some object, answer a morph can be thought of as being viewed;  A gesture is made toward generalizing this beyond the morph/player regime, in that a plain blue rectangle is returned rather than simply failing if the referent is not itself displayable."

	objectToView ifNil: [^ nil].
	^ objectToView isMorph
		ifTrue:
			[objectToView]
		ifFalse:
			[Morph new color: Color blue]
