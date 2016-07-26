rendering
morph
	|morph|
	 morph := RubScrolledTextMorph new
		beForSmalltalkCode;
		getSelectionSelector: #primarySelectionInterval;
		model: textModel;
		color: Smalltalk ui theme backgroundColor;
		textFont: StandardFonts codeFont;
		yourself.
	^ morph