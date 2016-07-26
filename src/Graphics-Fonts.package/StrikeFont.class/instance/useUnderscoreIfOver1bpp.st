character shapes
useUnderscoreIfOver1bpp

	glyphs depth = 1 ifTrue: [
		characterToGlyphMap ifNotNil: [	
			characterToGlyphMap at: 96 put: 95.
			characterToGlyphMap at: 95 put: 94 ].
		^self ].
	
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 129.
	characterToGlyphMap at: 95 put: 128