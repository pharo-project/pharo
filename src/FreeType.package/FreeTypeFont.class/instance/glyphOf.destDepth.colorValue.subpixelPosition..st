glyph lookup
glyphOf: aCharacter destDepth: destDepth colorValue: aColorValue subpixelPosition: sub
	"sub can be between 0 and 63 and denotes the sub-pixel position of the glyph"
	| validSub |
	validSub := self isSubPixelPositioned
		ifTrue: [((sub asInteger max: 0) min: 63) "bitAnd: 2r111000"]
		ifFalse:[0].
	^(destDepth >=8 and:[FreeTypeSettings current subPixelAntiAliasing])
		ifTrue:[
			self 
				subGlyphOf: aCharacter
				colorValue: aColorValue
				mono: FreeTypeSettings current monoHinting
				subpixelPosition: validSub]
		ifFalse:[
			(destDepth >= 8 and:[FreeTypeSettings current useSubPixelAntiAliasing])
				ifTrue:[
					self 
						mode41GlyphOf: aCharacter
						colorValue: aColorValue
						mono: FreeTypeSettings current monoHinting
						subpixelPosition: validSub]
				ifFalse:[
					self 
						glyphOf: aCharacter
						colorValue: aColorValue
						mono: FreeTypeSettings current monoHinting
						subpixelPosition: validSub]]