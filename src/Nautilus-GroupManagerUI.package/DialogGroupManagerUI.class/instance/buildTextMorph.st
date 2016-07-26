display
buildTextMorph
	| textMorph |
	textMorph := self textModel newScrolledText
		beForPlainText;
		beReadOnly;
		hResizing: #spaceFill;
		yourself.
	self textModel setInitialText: self text.
	^ textMorph