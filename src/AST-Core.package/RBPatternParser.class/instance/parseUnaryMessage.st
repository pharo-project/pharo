private-parsing
parseUnaryMessage
	| node |
	node := self parsePrimitiveObject.
	self addCommentsTo: node.
	
	[currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	currentToken isPatternBlock 
		ifTrue: 
			[node := self parseWrapperPatternBlockWith: node ].
	currentToken isIdentifier] 
			whileTrue: [
				currentToken isKeywordPattern
					ifTrue: [ self parserError: ' incomplete keyword pattern ' ].
				node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node