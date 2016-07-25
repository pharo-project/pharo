parsing
parseCharOrRange

	| firstChar |
	firstChar := lookahead.
	self match: firstChar.
	lookahead = $- ifTrue:
		[self match: $-.
		lookahead isNil
			ifTrue: [^self addChar: firstChar; addChar: $-]
			ifFalse: 
				[self addRangeFrom: firstChar to: lookahead.
				^self match: lookahead]].
	self addChar: firstChar