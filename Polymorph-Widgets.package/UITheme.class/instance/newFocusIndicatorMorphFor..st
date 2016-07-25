morph creation
newFocusIndicatorMorphFor: aMorph
	"Answer a new focus indicator for the given morph."

	^BorderedMorph new
		fillStyle: Color transparent;
		borderStyle: (DashedBorder
						width: 1
						dashColors: {aMorph focusColor. Color transparent} dashLengths: #(1 1));
		bounds: aMorph focusBounds