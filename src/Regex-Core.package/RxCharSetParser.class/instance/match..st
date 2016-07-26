parsing
match: aCharacter

	aCharacter = lookahead
		ifFalse: [
			RxParser 
				signalSyntaxException: 'unexpected character: ', (String with: lookahead)
				at: source position].
	^source atEnd
		ifTrue: [lookahead := nil]
		ifFalse: [lookahead := source next]