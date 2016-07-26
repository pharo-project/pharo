accessing
indexOf: aCharacter startingAt: start

	(aCharacter isCharacter) ifFalse: [^ 0].
	^ self class indexOfAscii: aCharacter asciiValue inString: self startingAt: start