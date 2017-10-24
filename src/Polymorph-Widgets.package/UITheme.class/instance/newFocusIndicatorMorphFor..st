morph creation
newFocusIndicatorMorphFor: aMorph
	"Answer a new focus indicator for the given morph."

	|radius|
	radius := aMorph focusIndicatorCornerRadius.
	^BorderedMorph new
		fillStyle: Color transparent;
		borderStyle: ((CompositeBorder new width: radius)
						borders: {RoundedBorder new
									cornerRadius: radius;
									width: 1;
									baseColor: self settings scrollbarColor.
								RoundedBorder new	
									cornerRadius: radius - 1;
									width: 1;
									baseColor: (self settings scrollbarColor alpha: 0.4)});
		bounds: aMorph focusBounds