accessing method dictionary
nextQuotePosIn: sourceString startingFrom: commentStart
	| pos nextQuotePos |
	pos := commentStart + 1.
	[((nextQuotePos := sourceString findString: '"' startingAt: pos) == (sourceString findString: '""' startingAt: pos)) and: [nextQuotePos ~= 0]]
		whileTrue:
			[pos := nextQuotePos + 2].
	^nextQuotePos