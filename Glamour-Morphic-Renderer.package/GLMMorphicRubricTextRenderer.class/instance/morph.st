rendering
morph
	^ RubScrolledTextMorph new
		getSelectionSelector: #primarySelectionInterval;
		color: Smalltalk ui theme backgroundColor;
		textColor: Smalltalk ui theme  textColor;
		textFont: StandardFonts defaultFont;
		model: textModel;
		yourself.