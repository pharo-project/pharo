parse
parseCharSymbol
	| s e |
	s := sourcePosition - 1.
	e := sourcePosition.
	self nextChar.
	self
		scanPast: #symbol
		start: s
		end: e