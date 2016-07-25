ui
readOnlyView
	^ RubScrolledTextMorph new
		setTextWith: self getText;
		wrapped: self textArea wrapped;
		width: 200;
		yourself