glyph lookup
glyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub

	| |
	^FreeTypeCache current
		atFont: self
		charCode: aCharacter asUnicode asInteger
		type: ((1+sub) << 32) + aColorValue
		ifAbsentPut: [
			FreeTypeGlyphRenderer current
				glyphOf: aCharacter 
				colorValue: aColorValue 
				mono: monoBoolean 
				subpixelPosition: sub 
				font: self]

