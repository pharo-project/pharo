character shapes
makeLfVisible
	| glyph |
	self characterToGlyphMap.
	glyph := self characterFormAt: (Character value: 163).
	glyph 
		border: glyph boundingBox
		width: 1
		fillColor: Color blue.
	"	glyph := glyph reverse."
	self 
		characterFormAt: (Character value: 132)
		put: glyph.
	characterToGlyphMap 
		at: 11
		put: 132