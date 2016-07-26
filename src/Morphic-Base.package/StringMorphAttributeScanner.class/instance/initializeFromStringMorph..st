string morph
initializeFromStringMorph: aStringMorph

	fontNumber := 1.
	self setActualFont: (aStringMorph font ifNil: [ TextStyle defaultFont ]).
	
	emphasis := actualFont emphasis.
	textColor := aStringMorph color.
