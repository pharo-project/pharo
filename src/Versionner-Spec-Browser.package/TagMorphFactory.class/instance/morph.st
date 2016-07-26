protocol
morph
	| textMorph width |
	textMorph := self textMorph.
	width := textMorph width + 10.
	^ BorderedMorph new
		extent: width @ self height;
		borderWidth: 1;
		color: self color;
		useRoundedCorners;
		addMorphCentered: textMorph;
		yourself