accessing
glyphsEncoding: anInteger

	^ (fontArray at: (anInteger+1)) glyphs.
