private-parsing
parseMessagePattern
	currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	^currentToken isIdentifier 
		ifTrue: [self parseUnaryPattern]
		ifFalse: 
			[currentToken isKeyword 
				ifTrue: [self parseKeywordPattern]
				ifFalse: [self parseBinaryPattern]]