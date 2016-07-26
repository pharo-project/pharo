measuring
getWidthOf: aCharacter 
	"Glyphs are either 1 or 8 bit deep. For 32 bpp we use 8 bits, otherwise 1"
	| em glyph hintingFlags flags charCode |

	aCharacter < $  ifTrue: [^self getWidthOf: $ ].
	charCode := aCharacter asUnicode asInteger.
	(self face charmaps includes:'unic')
		ifTrue:[
			(self isSymbolFont and:[ charCode between: 16r20 and: 16rFF ])
				ifTrue:[charCode := charCode + 16rF000]]
		ifFalse:[
			(self face charmaps includes:'armn')
				ifTrue:[ "select apple roman char map, and map character from unicode to mac encoding"
					self face setCharMap:'armn'.
					charCode := aCharacter unicodeToMacRoman asUnicode asInteger. "check this!"]].
	em := self pixelSize.
	face validate.
	face isValid ifFalse:[^0].
	face setPixelWidth: em height: em.
	hintingFlags := FreeTypeSettings current hintingFlags.
	flags :=  LoadNoBitmap bitOr:( LoadIgnoreTransform bitOr: hintingFlags). 
	[face loadCharacter: charCode flags: flags.
	] on:FT2Error do:[:e | "character not in map?"^0]. 
	glyph := face glyph.
	"When not hinting FreeType sets the advance to the truncated linearAdvance.
	The characters appear squashed together. Rounding is probably better, so we 
	answer the rounded linear advance here"
	^self subPixelPositioned
		ifTrue:[ glyph roundedPixelLinearAdvance x]
		ifFalse:[ glyph advance x].
