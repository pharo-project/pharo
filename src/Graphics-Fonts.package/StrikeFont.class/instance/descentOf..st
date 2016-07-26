accessing
descentOf: aCharacter

	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont descentOf: aCharacter.
		].
	].
	^ self descent.
