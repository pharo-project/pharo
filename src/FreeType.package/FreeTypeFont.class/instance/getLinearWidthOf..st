measuring
getLinearWidthOf: aCharacter 
	| em glyph la charCode |

	aCharacter < $  ifTrue: [^self getLinearWidthOf: $ ].
	charCode := aCharacter asUnicode asInteger.
	(self face charmaps includes:'unic')
		ifTrue:[
			(self isSymbolFont and: [ charCode between: 16r20 and: 16rFF ])
				ifTrue:[charCode := charCode + 16rF000]]
		ifFalse:[
			(self face charmaps includes:'armn')
				ifTrue:[ "select apple roman char map, and map character from unicode to mac encoding"
					self face setCharMap:'armn'.
					charCode := aCharacter unicodeToMacRoman asUnicode asInteger. "check this!"]].
	em := self pixelSize.
	face validate.
	face setPixelWidth: em height: em.
	[face loadCharacter: charCode flags: (LoadNoBitmap bitOr: (LoadIgnoreTransform bitOr: "FreeTypeSettings current hintingFlags" 2 "no hinting"))]
		on: FT2Error do:[:e |
			face loadGlyph: 0 flags: (LoadNoBitmap bitOr: (LoadIgnoreTransform bitOr: FreeTypeSettings current hintingFlags "no hinting")) ].
	glyph := face glyph.
	la := glyph linearHorizontalAdvance.
	la isZero ifTrue:[
		"FreeType 2.2.1 sometimes screws up when getting metrics,
		Maybe the bug is in the plugin?
		For example Calibri pixel size 13 gives linearAdvance x of zero !
		We try again at double the size, and half the result"
		em := self pixelSize * 2.
		face validate.
		face setPixelWidth: em height: em.
		face loadCharacter: charCode flags:(LoadNoBitmap bitOr: (LoadIgnoreTransform bitOr: "FreeTypeSettings current hintingFlags" 2 "no hinting")). "load glyph metrics"
		glyph := face glyph.
		la := glyph linearHorizontalAdvance / 2.0].
	^la
