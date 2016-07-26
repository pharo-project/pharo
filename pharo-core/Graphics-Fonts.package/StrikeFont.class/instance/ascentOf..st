accessing
ascentOf: aCharacter

	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont ascentOf: aCharacter.
		].
	].
	^ self ascent.
