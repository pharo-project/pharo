parsing
parseFaultyExpression: aString 
	^self parseExpression: aString onError: self errorNodeBlock