accessing
parse

	lookahead = $- ifTrue:
		[self addChar: $-.
		self match: $-].
	[lookahead isNil] whileFalse: [self parseStep].
	^elements