events
focused: aBoolean from: aTextMorph
	super focused: aBoolean from: aTextMorph.
	selectorChooser ifNotNil: [selectorChooser comeToFront]