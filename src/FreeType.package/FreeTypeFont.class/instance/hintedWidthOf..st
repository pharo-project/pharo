measuring
hintedWidthOf: aCharacter
	"retrieve advance width for character. try to use cached glyph if possible"
	| charCode answer |
	
	charCode := aCharacter asUnicode asInteger.
	answer := FreeTypeCache current 
		atFont: self 
		charCode: charCode 
		type: FreeTypeCacheWidth
		ifAbsentPut: [self getWidthOf: aCharacter].
	^answer
