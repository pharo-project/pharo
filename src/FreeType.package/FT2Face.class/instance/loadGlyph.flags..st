glyphs
loadGlyph: index flags: flags
	self primLoadGlyph: index flags: flags.
	glyph
		ifNil: [ glyph := FT2GlyphSlot fromFace: self ]
		ifNotNil: [ glyph loadFrom: self ].
