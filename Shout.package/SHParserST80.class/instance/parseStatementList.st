parse
parseStatementList
	[[currentTokenFirst == $.]
		whileTrue: [self scanPast: #statementSeparator].
	(currentToken notNil
			and: [currentTokenFirst ~~ $]])
		ifTrue: [self parseStatement].
	currentTokenFirst == $.]
		whileTrue: [self scanPast: #statementSeparator]