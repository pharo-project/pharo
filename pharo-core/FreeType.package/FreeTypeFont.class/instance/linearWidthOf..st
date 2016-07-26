measuring
linearWidthOf: aCharacter
	"retrieve linear advance width for character. try to use cached glyph if possible.
	This is the scaled, unrounded advance width."
	| charCode answer |
	
	charCode := aCharacter asUnicode asInteger.
	answer := FreeTypeCache current 
		atFont: self 
		charCode: charCode 
		type: FreeTypeCacheLinearWidth
		ifAbsentPut: [self getLinearWidthOf: aCharacter].
	^answer
