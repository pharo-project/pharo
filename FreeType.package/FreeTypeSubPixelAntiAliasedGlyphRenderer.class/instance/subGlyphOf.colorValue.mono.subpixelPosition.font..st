rendering
subGlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont

	| f |
	monoBoolean
		ifFalse:[
			f := self 
				renderStretchedGlyph: aCharacter 
				depth: 8 
				subpixelPosition: sub 
				font: aFreeTypeFont. 
			f := self filter: f]
		ifTrue:[
			f := self 
				renderGlyph: aCharacter 
				depth: 1 
				subpixelPosition: sub 
				font: aFreeTypeFont.
			f := self fixBytesForMono: f.					
			f := f asFormOfDepth: 32].
	^f