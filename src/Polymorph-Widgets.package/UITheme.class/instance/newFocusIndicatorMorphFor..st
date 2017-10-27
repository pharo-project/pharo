morph creation
newFocusIndicatorMorphFor: aMorph
	"Answer a new focus indicator for the given morph."

	^ BorderedMorph new
		fillStyle: Color transparent;
		borderStyle: (SimpleBorder new
			 width: 1;
			baseColor: (self glamorousBaseSelectionColorFor: aMorph));
		bounds: aMorph focusBounds