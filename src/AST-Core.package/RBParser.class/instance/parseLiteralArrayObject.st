private-parsing
parseLiteralArrayObject
	currentToken isSpecial 
		ifTrue: 
			[currentToken value = $( ifTrue: [^self parseLiteralArray].
			"currentToken value == $[ ifTrue: [^self parseLiteralByteArray]"].
	currentToken isLiteralArrayToken 
		ifTrue: 
			[^currentToken isForByteArray 
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	currentToken isError ifTrue:[ |errorNode| errorNode :=  self parserError: currentToken cause.
		self step. ^ errorNode].
	currentToken isLiteralToken ifFalse: [self patchLiteralArrayToken].
	^self parsePrimitiveLiteral