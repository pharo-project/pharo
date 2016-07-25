scan
scanNext
	self scanWhitespace.
	currentTokenFirst := self currentChar.
	currentTokenFirst isNil
		ifTrue: [currentTokenFirst := Character space.
			currentTokenSourcePosition := nil.
			currentToken := nil.
			^ nil].
	currentTokenFirst isDigit
		ifTrue: [^ self scanNumber].
	(currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		ifTrue: [^ self scanIdentifier].
	^ self scanBinary