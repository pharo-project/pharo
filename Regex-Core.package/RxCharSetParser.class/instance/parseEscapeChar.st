parsing
parseEscapeChar

	self match: $\.
	$- = lookahead
		ifTrue: [elements add: (RxsCharacter with: $-)]
		ifFalse: [elements add: (RxsPredicate forEscapedLetter: lookahead)].
	self match: lookahead