glyphs
loadCharacter: index flags: flags
	self primLoadCharacter: index flags: flags.
	glyph
		ifNil: [ glyph := FT2GlyphSlot fromFace: self ]
		ifNotNil: [ glyph loadFrom: self ].
