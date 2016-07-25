parse
parseString
	| first c answer last |
	first := sourcePosition.
	answer := ''.
	[(c := self currentChar) isNil
		ifTrue: [self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error].
	(c ~~ $'
			and: [answer := answer copyWith: c.
				true])
		or: [self peekChar == $'
				and: [sourcePosition := sourcePosition + 1.
					answer := answer copyWith: $'.
					true]]]
		whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last.
	^ answer