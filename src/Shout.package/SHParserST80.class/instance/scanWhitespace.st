scan
scanWhitespace
	| c |
	[c := self currentChar.
	c notNil
		and: [c isSeparator]]
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $"
		ifTrue: [self scanComment]