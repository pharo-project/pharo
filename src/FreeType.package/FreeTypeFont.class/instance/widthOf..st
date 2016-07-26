measuring
widthOf: aCharacter
	"retrieve advance width for character. try to use cached glyph if possible"
	^self isSubPixelPositioned
		ifTrue:[self linearWidthOf: aCharacter]
		ifFalse: [self hintedWidthOf: aCharacter]
