initialization
initializeTextMorph
	textMorph := RubScrolledTextMorph new.
	textMorph
		beReadOnly;
		beWrapped;
		textFont: self theme textFont;
		textColor: self theme textColor;
		backgroundColor: BalloonMorph balloonColor.
	self
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		addMorph: textMorph fullFrame: (LayoutFrame identity
			fractions: (0 @ 0 corner: 1 @ 1) offsets: (0 @ 0 corner: 0 @ 0); 
			yourself)