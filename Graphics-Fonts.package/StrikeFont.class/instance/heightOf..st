accessing
heightOf: aCharacter

	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont heightOf: aCharacter.
		].
	].
	^ self height.
