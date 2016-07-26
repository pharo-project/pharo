accessing
extentsOf: aString
	|glyphs|
	glyphs := self glyphsOf: aString.
   cairoFont getExtentsOfGlyphs: glyphs getHandle ofLength: glyphs size into: glyphExtents.
	^ glyphExtents