accessing
indexOf: aCharacter

	aCharacter isCharacter ifFalse: [^ 0].
	^ self class
		indexOfAscii: aCharacter asciiValue
		inString: self
		startingAt: 1.
