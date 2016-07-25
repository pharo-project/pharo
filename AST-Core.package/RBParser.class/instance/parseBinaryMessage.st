private-parsing
parseBinaryMessage
	| node |
	node := self parseUnaryMessage.
	
	[currentToken isLiteralToken ifTrue: [self patchNegativeLiteral].
	currentToken isBinary] 
			whileTrue: [node := self parseBinaryMessageWith: node].
	^node