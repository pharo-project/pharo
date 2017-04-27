initialization
initializeTextMorph
	textMorph := RubScrolledTextMorph new.
	textMorph
		beReadOnly;
		beWrapped;
		textFont: self theme textFont;
		textColor: self theme textColor;
		backgroundColor: self theme errorPopperBackgroundColor.
	self
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		addMorph: textMorph fullFrame: (LayoutFrame identity)