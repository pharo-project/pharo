glyph lookup
subGlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub

	| |
	^FreeTypeCache current
		atFont: self
		charCode: aCharacter asUnicode asInteger
		type: FreeTypeCacheGlyphLCD + sub
		ifAbsentPut: [
			FreeTypeGlyphRenderer current
				subGlyphOf: aCharacter 
				colorValue: aColorValue 
				mono: monoBoolean 
				subpixelPosition: sub 
				font: self]

