scan
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := c asString.
	d := self nextChar.
	((self isSelectorCharacter: c)
			or: [c == $:])
		ifFalse: [^ currentToken].
	(c == $:
			and: [d == $=])
		ifTrue: [currentToken := currentToken , d asString.
			self nextChar.
			^ currentToken].
	((c == $|) and:[self parsingBlockarguments])
		ifTrue: [^ currentToken].
	[self isSelectorCharacter: d]
		whileTrue: [currentToken := currentToken , d asString.
			d := self nextChar].
	^ currentToken