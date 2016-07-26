scan
scanNumber
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	c == $r
		ifTrue: [base := Integer
						readFrom: (ReadStream
								on: (source copyFrom: start to: sourcePosition - 1)).
			self peekChar == $-
				ifTrue: [self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c == $.
				ifTrue: [(self isBigDigit: self nextChar base: base)
						ifTrue: [self skipBigDigits: base]
						ifFalse: [sourcePosition := sourcePosition - 1]].
			c := self currentChar.
			('deq' includes: c)
				ifTrue: [((nc := self nextChar) isDigit
							or: [nc == $-
									and: [self peekChar isDigit]])
						ifTrue: [self skipDigits]
						ifFalse: [sourcePosition := sourcePosition - 1]].
			c == $s
				ifTrue: [(nc := self nextChar) isDigit
						ifTrue: [self skipDigits]
						ifFalse: [nc isLetter ifTrue: [sourcePosition := sourcePosition - 1]]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^ currentTokenSourcePosition := start].
	c == $s
		ifTrue: [(nc := self nextChar) isDigit
					ifTrue: [self skipDigits]
					ifFalse: [nc isLetter ifTrue: [sourcePosition := sourcePosition - 1]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^ currentTokenSourcePosition := start].
	c == $.
		ifTrue: [self nextChar isDigit
				ifTrue: [self skipDigits]
				ifFalse: [sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^ currentTokenSourcePosition := start]].
	c := self currentChar.
	('deq' includes: c)
		ifTrue: [((nc := self nextChar) isDigit
					or: [nc == $-
							and: [self peekChar isDigit]])
				ifTrue: [self skipDigits]
				ifFalse: [sourcePosition := sourcePosition - 1]].
	c == $s
		ifTrue: [(nc := self nextChar) isDigit
				ifTrue: [self skipDigits]
				ifFalse: [nc isLetter ifTrue: [sourcePosition := sourcePosition - 1]]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^ currentTokenSourcePosition := start