accessing
labelGraphic: aForm
	"Changed to look for any image morph rather than just a sketch."
	
	graphicalMorph ifNotNil: [graphicalMorph delete].
	graphicalMorph := ImageMorph new form: aForm.
	self extent: graphicalMorph extent + (self borderWidth + self extraBorder).
	graphicalMorph position: self center - (graphicalMorph extent // 2).
	self addMorph: graphicalMorph.
	graphicalMorph lock