parse
parseSymbol
	| c |
	currentToken = '#'
		ifTrue: [self rangeType: #symbol.
			self scanWhitespace].
	c := self currentChar.
	self
		failWhen: (c isNil
				or: [c isSeparator]).
	c == $(
		ifTrue: [self nextChar.
			self
				scanPast: #arrayStart
				start: currentTokenSourcePosition
				end: currentTokenSourcePosition + 1.
			^ self parseArray].
	c == $[
		ifTrue: [self nextChar.
			self
				scanPast: #byteArrayStart
				start: currentTokenSourcePosition
				end: currentTokenSourcePosition + 1.
			^ self parseByteArray].
	c == $'
		ifTrue: [^ self parseSymbolString].
	((self isSelectorCharacter: c)
			or: [c == $-])
		ifTrue: [^ self parseSymbolSelector].
	(c isLetter
			or: [c == $:])
		ifTrue: [^ self parseSymbolIdentifier].
	^ self parseCharSymbol