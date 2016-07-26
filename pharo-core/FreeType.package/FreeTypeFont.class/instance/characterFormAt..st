glyph lookup
characterFormAt: aCharacter
	FreeTypeSettings current 
		forceNonSubPixelDuring:[
			^self 
				glyphOf: aCharacter 
				destDepth: 32 
				colorValue: (Color black pixelValueForDepth: 32)
				subpixelPosition: 0]