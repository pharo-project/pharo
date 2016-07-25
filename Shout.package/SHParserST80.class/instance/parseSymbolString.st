parse
parseSymbolString
	| first c last |
	first := sourcePosition.
	self nextChar.
	[(c := self currentChar) isNil
		ifTrue: [self
				rangeType: #unfinishedString
				start: first
				end: source size.
			self error].
	c ~~ $'
		or: [self peekChar == $'
				and: [sourcePosition := sourcePosition + 1.
					true]]]
		whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self
		scanPast: #stringSymbol
		start: first - 1
		end: last