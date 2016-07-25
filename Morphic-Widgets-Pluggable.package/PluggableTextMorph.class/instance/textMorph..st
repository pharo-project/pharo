editor access
textMorph: aTextMorph
	textMorph ifNotNil: [ 
		scrollBar removeMorph: textMorph ].
	textMorph := aTextMorph.
	self configureTextMorph: textMorph.