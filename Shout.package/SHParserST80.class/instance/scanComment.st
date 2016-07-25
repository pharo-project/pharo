scan
scanComment
	| c s e |
	s := sourcePosition.
	[sourcePosition := sourcePosition + 1.
	(c := self currentChar)
		ifNil: [self
				rangeType: #unfinishedComment
				start: s
				end: source size.
			^ self error].
	c == $"] whileFalse.
	e := sourcePosition.
	s < e
		ifTrue: [self
				rangeType: #comment
				start: s
				end: e].
	self nextChar.
	self scanWhitespace