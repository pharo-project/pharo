glyph lookup
mode41GlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub

	| |
	^FreeTypeCache current
		atFont: self
		charCode: aCharacter asUnicode asInteger
		type: (FreeTypeCacheGlyph + sub)
		ifAbsentPut: [
			FreeTypeGlyphRenderer current
				mode41GlyphOf: aCharacter 
				colorValue: aColorValue 
				mono: monoBoolean 
				subpixelPosition: sub 
				font: self]

