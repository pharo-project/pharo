private-parsing
parsePragmaLiteral
	currentToken isLiteralArrayToken 
		ifTrue: 
			[^currentToken isForByteArray 
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	currentToken isLiteralToken ifFalse: [self parserError:'Literal constant expected'].
	^self parsePrimitiveLiteral