parse
parseSymbolSelector
	| start end |
	start := sourcePosition - 1.
	end := sourcePosition.
	[self isSelectorCharacter: self nextChar]
		whileTrue: [end := sourcePosition].
	self
		scanPast: #symbol
		start: start
		end: end