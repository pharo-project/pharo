accessing
getGlyphWidth: aCharacter
	cairoFont getExtentsOf: (utfConverter convertChar: aCharacter) into: glyphExtents.
	^ glyphExtents x_advance