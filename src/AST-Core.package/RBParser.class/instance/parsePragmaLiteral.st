private-parsing
parsePragmaLiteral
	currentToken isSpecial 
		ifTrue: 
			[currentToken value = $( ifTrue: [^self parseLiteralArray]].
	currentToken isLiteralArrayToken 
		ifTrue: 
			[^currentToken isForByteArray 
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	currentToken isLiteralToken ifFalse: [self parserError:'Literal constant expected'].
	^self parsePrimitiveLiteral