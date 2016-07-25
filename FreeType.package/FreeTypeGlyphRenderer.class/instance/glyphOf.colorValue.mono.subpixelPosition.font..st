public
glyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont

	| f |
	f := self 
		renderGlyph: aCharacter 
		depth: (monoBoolean ifTrue:[1] ifFalse:[8]) 
		subpixelPosition: sub
		font: aFreeTypeFont.
	monoBoolean 
		ifTrue:[
			f := self fixBytesForMono: f.
			f := f asFormOfDepth: 8].
	f := self convert8to32: f colorValue: aColorValue.
	^f
	