rendering
renderCharacters: aString from: start to: end
	| glyphs |

	glyphs := self glyphsOf: aString from: start to: end.
	self renderGlyphs: glyphs.
	^ self advance